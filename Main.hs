module Main ( main ) where

import Hledger.Interest
import Hledger.Query
import Hledger.Read
import Hledger.Utils ( toRegex' )

import Control.Exception ( bracket )
import Control.Monad
import Data.List ( sortOn )
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Version
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Paths_hledger_interest ( version )

data Options = Options
  { optVerbose          :: Bool
  , optShowVersion      :: Bool
  , optShowHelp         :: Bool
  , optInput            :: [FilePath]
  , optSourceAcc        :: String
  , optTargetAcc        :: String
  , optDCC              :: Maybe DayCountConvention
  , optRate             :: Maybe Rate
  , optBalanceToday     :: Bool
  , optBalanceUntil     :: Maybe String
  , optIgnoreAssertions :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose          = True
  , optShowVersion      = False
  , optShowHelp         = False
  , optInput            = []
  , optSourceAcc        = ""
  , optTargetAcc        = ""
  , optDCC              = Nothing
  , optRate             = Nothing
  , optBalanceToday     = False
  , optBalanceUntil     = Nothing
  , optIgnoreAssertions = False
  }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['h'] ["help"]              (NoArg (\o -> o { optShowHelp = True }))                             "print this message and exit"
 , Option ['V'] ["version"]           (NoArg (\o -> o { optShowVersion = True }))                          "show version number and exit"
 , Option ['v'] ["verbose"]           (NoArg (\o -> o { optVerbose = True }))                              "echo input ledger to stdout (default)"
 , Option ['q'] ["quiet"]             (NoArg (\o -> o { optVerbose = False }))                             "don't echo input ledger to stdout"
 , Option []    ["today"]             (NoArg (\o -> o { optBalanceToday = True }))                         "compute interest up until today"
 , Option []    ["until"]             (ReqArg (\d o -> o { optBalanceUntil = Just d}) "YYYY-MM-DD")        "compute interest up until the given date"
 , Option ['f'] ["file"]              (ReqArg (\f o -> o { optInput = f : optInput o }) "FILE")            "input ledger file (pass '-' for stdin)"
 , Option ['s'] ["source"]            (ReqArg (\a o -> o { optSourceAcc = a }) "ACCOUNT")                  "interest source account"
 , Option ['t'] ["target"]            (ReqArg (\a o -> o { optTargetAcc = a }) "ACCOUNT")                  "interest target account"
 , Option ['I'] ["ignore-assertions"] (NoArg (\o -> o { optIgnoreAssertions = True }))                     "ignore any failing balance assertions"
 , Option []    ["act"]               (NoArg (\o -> o { optDCC = Just diffAct }))                          "use 'act' day counting convention"
 , Option []    ["30-360"]            (NoArg (\o -> o { optDCC = Just diff30_360 }))                       "use '30/360' day counting convention"
 , Option []    ["30E-360"]           (NoArg (\o -> o { optDCC = Just diff30E_360 }))                      "use '30E/360' day counting convention"
 , Option []    ["30E-360isda"]       (NoArg (\o -> o { optDCC = Just diff30E_360isda }))                  "use '30E/360isda' day counting convention"
 , Option []    ["constant"]          (ReqArg (\r o -> o { optRate = Just (constant (read r)) }) "RATE")   "constant interest rate"
 , Option []    ["annual"]            (ReqArg (\r o -> o { optRate = Just (perAnno (read r)) }) "RATE")    "annual interest rate"
 , Option []    ["annual-schedule"]   (ReqArg (\r o -> o { optRate = Just (perAnnoSchedule (read r)) })    "SCHEDULE")    "schedule of annual interest rates.\nsyntax: '[(Date1,Rate1),(Date2,Rate2),...]'"
 , Option []    ["bgb288"]            (NoArg (\o -> o { optRate = Just bgb288, optDCC = Just diffAct }))   "compute interest according to German BGB288"
 , Option []    ["db24"]              (NoArg (\o -> o { optRate = Just db24, optDCC = Just diff30E_360 })) "HACK: Deutsche Bank 24"
 , Option []    ["ing-diba"]          (NoArg (\o -> o { optRate = Just ingDiba, optDCC = Just diffAct }))  "HACK: compute interest according for Ing-Diba Tagesgeld account"
 ]

usageMessage :: String
usageMessage = usageInfo header options
  where header = "Usage: hledger-interest [OPTION...] ACCOUNT"

commandLineError :: String -> IO a
commandLineError err = do hPutStrLn stderr (err ++ usageMessage)
                          exitFailure

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> commandLineError (concat errs)

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  (opts, args) <- getArgs >>= parseOpts
  when (optShowVersion opts) (putStrLn (showVersion version) >> exitSuccess)
  when (optShowHelp opts) (putStr usageMessage >> exitSuccess)
  when (null (optSourceAcc opts)) (commandLineError "required --source option is missing\n")
  when (null (optTargetAcc opts)) (commandLineError "required --target option is missing\n")
  when (isNothing (optDCC opts)) (commandLineError "no day counting convention specified\n")
  when (isNothing (optRate opts)) (commandLineError "no interest rate specified\n")
  mbComputeInterestUntil <-
    case optBalanceUntil opts of
      Just untilStr
        | optBalanceToday opts ->
            commandLineError "Specify either --today or --until=YYYY-MM-DD.\n"
        | otherwise -> do
            let fmt = "%Y-%-m-%-d"
            case parseTimeM True defaultTimeLocale fmt untilStr :: Maybe Day of
              Nothing -> commandLineError $ "Can't parse the specified --until date." ++
                " Make sure it has the format " ++ fmt ++ ".\n"
              Just day -> pure $ Just day
      Nothing
        | optBalanceToday opts -> Just <$> getCurrentDay
        | otherwise -> return Nothing
  let ledgerInputOptions = definputopts { balancingopts_ = (balancingopts_ definputopts) { ignore_assertions_ = optIgnoreAssertions opts } }
  jnl' <- runExceptT (readJournalFiles ledgerInputOptions (reverse (optInput opts))) >>= either fail return
  interestAcc <- case args of
                   []    -> commandLineError "required argument ACCOUNT is missing\n"
                   [acc] -> return $ T.pack acc
                   _     -> commandLineError "only one interest ACCOUNT may be specified\n"
  let jnl = filterJournalTransactions (Acct (toRegex' interestAcc)) jnl'
      ts  = sortOn tdate (jtxns jnl)
      cfg = Config
            { interestAccount = interestAcc
            , sourceAccount = T.pack (optSourceAcc opts)
            , targetAccount = T.pack (optTargetAcc opts)
            , dayCountConvention = fromJust (optDCC opts)
            , interestRate = fromJust (optRate opts)
            }
  let ts' = runComputer cfg $ do
        mapM_ processTransaction ts
        mapM_ computeInterest mbComputeInterestUntil
      result
        | optVerbose opts = ts' ++ ts
        | otherwise       = ts'
  mapM_ (putStr . T.unpack . showTransaction) (sortOn tdate result)
