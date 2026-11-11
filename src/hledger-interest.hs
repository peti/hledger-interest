module Main ( main ) where

import Hledger.Interest
import Hledger.Read

import Control.Exception ( ErrorCall(..), finally, handle )
import Control.Monad
import Data.List ( sortOn )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Read (readEither)

import Paths_hledger_interest ( version )

data Options = Options
  { optVerbose          :: Bool
  , optShowVersion      :: Bool
  , optShowHelp         :: Bool
  , optInput            :: [FilePath]
  , optSourceAcc        :: String
  , optTargetAcc        :: String
  , optDCC              :: Maybe DayCountConvention
  , optRate             :: Either String Rate
  , optBalanceToday     :: Bool
  , optPostOnTxns       :: Bool
  , optPostAtYearEnd    :: Bool
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
  , optRate             = Left "no interest rate specified\n"
  , optBalanceToday     = False
  , optPostOnTxns       = False
  , optPostAtYearEnd    = True
  , optIgnoreAssertions = False
  }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['h'] ["help"]              (NoArg (\o -> o { optShowHelp = True }))                             "print this message and exit"
 , Option ['V'] ["version"]           (NoArg (\o -> o { optShowVersion = True }))                          "show version number and exit"
 , Option ['v'] ["verbose"]           (NoArg (\o -> o { optVerbose = True }))                              "echo input ledger to stdout (default)"
 , Option ['q'] ["quiet"]             (NoArg (\o -> o { optVerbose = False }))                             "don't echo input ledger to stdout"
 , Option []    ["today"]             (NoArg (\o -> o { optBalanceToday = True }))                         "compute interest up until today"
 , Option []    ["post-on-transactions"] (NoArg (\o -> o { optPostOnTxns = True }))                        "post interest only when the balance changes\n(and with --today), not at the end of\nevery year or rate period"
 , Option []    ["no-year-end-posting"] (NoArg (\o -> o { optPostAtYearEnd = False }))                     "skip automatic year-end postings;\nstill post at rate changes and transactions"
 , Option ['f'] ["file"]              (ReqArg (\f o -> o { optInput = f : optInput o }) "FILE")            "input ledger file (pass '-' for stdin)\ndefault: $LEDGER_FILE or ~/.hledger.journal"
 , Option ['s'] ["source"]            (ReqArg (\a o -> o { optSourceAcc = a }) "ACCOUNT")                  "interest source account"
 , Option ['t'] ["target"]            (ReqArg (\a o -> o { optTargetAcc = a }) "ACCOUNT")                  "interest target account"
 , Option ['I'] ["ignore-assertions"] (NoArg (\o -> o { optIgnoreAssertions = True }))                     "ignore any failing balance assertions"
 , Option []    ["act"]               (NoArg (\o -> o { optDCC = Just diffAct }))                          "use 'act/act' day counting convention\n(Actual/Actual ISDA; see README)"
 , Option []    ["30-360"]            (NoArg (\o -> o { optDCC = Just diff30_360 }))                       "use '30/360' day counting convention"
 , Option []    ["30E-360"]           (NoArg (\o -> o { optDCC = Just diff30E_360 }))                      "use '30E/360' day counting convention"
 , Option []    ["30E-360isda"]       (NoArg (\o -> o { optDCC = Just diff30E_360isda }))                  "use '30E/360isda' day counting convention"
 , Option []    ["constant"]          (ReqArg (setRate "--constant" constant) "RATE")                    "fixed annual interest rate"
 , Option []    ["annual"]            (ReqArg (setRate "--annual" perAnno) "RATE")                        "alias for --constant"
 , Option []    ["schedule"]          (ReqArg (setRate "--schedule" scheduled) "SCHEDULE")                "schedule of annual interest rates.\nsyntax: '[(Date1,Rate1),(Date2,Rate2),...]'"
 , Option []    ["annual-schedule"]   (ReqArg (setRate "--annual-schedule" scheduled) "SCHEDULE")         "alias for --schedule"
 ]

setRate :: Read a => String -> (a -> Rate) -> String -> Options -> Options
setRate flag makeRate value opts = opts { optRate = parsed }
  where
    parsed = case readEither value of
      Left _ -> Left ("invalid value for " ++ flag ++ ": " ++ value ++ "\n")
      Right r -> Right (makeRate r)

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
main = handle (\(ErrorCall msg) -> hPutStrLn stderr msg >> exitFailure) $
  run `finally` (hFlush stdout >> hFlush stderr)

run :: IO ()
run = do
  (opts, args) <- getArgs >>= parseOpts
  when (optShowVersion opts) (putStrLn (showVersion version) >> exitSuccess)
  when (optShowHelp opts) (putStr usageMessage >> exitSuccess)
  when (null (optSourceAcc opts)) (commandLineError "required --source option is missing\n")
  when (null (optTargetAcc opts)) (commandLineError "required --target option is missing\n")
  dcc <- maybe (commandLineError "no day counting convention specified\n") pure (optDCC opts)
  rate <- either commandLineError pure (optRate opts)
  interestAcc <- case args of
                   []    -> commandLineError "required argument ACCOUNT is missing\n"
                   [acc] -> return $ T.pack acc
                   _     -> commandLineError "only one interest ACCOUNT may be specified\n"
  let ledgerInputOptions = definputopts { balancingopts_ = (balancingopts_ definputopts) { ignore_assertions_ = optIgnoreAssertions opts } }
  inputFiles <- case optInput opts of
                  [] -> pure <$> defaultJournalPath
                  fs -> return (reverse fs)
  jnl <- runExceptT (readJournalFiles ledgerInputOptions inputFiles) >>=
           either (\msg -> hPutStrLn stderr msg >> exitFailure) pure
  let ts = sortOn transactionDate2 $
             filter (any ((== interestAcc) . paccount) . tpostings) (jtxns jnl)
      cfg = Config
            { interestAccount = interestAcc
            , sourceAccount = T.pack (optSourceAcc opts)
            , targetAccount = T.pack (optTargetAcc opts)
            , dayCountConvention = dcc
            , interestRate = rate
            , postOnTransactions = optPostOnTxns opts
            , postAtYearEnd = optPostAtYearEnd opts
            }
  when (null ts) $ commandLineError ("no transactions found for account " ++ T.unpack interestAcc ++ "\n")
  thisDay <- getCurrentDay
  let finalize
        | optBalanceToday opts = computeInterest thisDay
        | otherwise            = return ()
      ts' = runComputer cfg (mapM_ processTransaction ts >> finalize)
      result
        | optVerbose opts = ts' ++ ts
        | otherwise       = ts'
  mapM_ (TIO.putStr . showTransaction) (sortOn transactionDate2 result)
