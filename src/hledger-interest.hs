module Main ( main ) where

import Hledger.Interest
import Hledger.Interest.ScheduleFile (readScheduleFile)
import Hledger.Read

import Control.Exception ( ErrorCall(..), finally, handle )
import Control.Monad
import Data.List ( sortOn )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.Version
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Read (readEither)

import Paths_hledger_interest ( version )

data RateInput = RateValue Rate | RateFile FilePath

data Options = Options
  { optVerbose          :: Bool
  , optShowVersion      :: Bool
  , optShowHelp         :: Bool
  , optInput            :: [FilePath]
  , optSourceAcc        :: String
  , optTargetAcc        :: String
  , optDCC              :: Maybe DayCountConvention
  , optRate             :: Either String RateInput
  , optBalanceUntil     :: Maybe String
  , optPostingMode      :: PostingMode
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
  , optBalanceUntil     = Nothing
  , optPostingMode      = PostAtAllBoundaries
  , optIgnoreAssertions = False
  }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['h'] ["help"]              (NoArg (\o -> o { optShowHelp = True }))                             "print this message and exit"
 , Option ['V'] ["version"]           (NoArg (\o -> o { optShowVersion = True }))                          "show version number and exit"
 , Option ['v'] ["verbose"]           (NoArg (\o -> o { optVerbose = True }))                              "print matching input transactions and generated interest (default)"
 , Option ['q'] ["quiet"]             (NoArg (\o -> o { optVerbose = False }))                             "print generated interest only"
 , Option []    ["today"]             (NoArg (\o -> o { optBalanceUntil = Just "today" }))                 "alias for --until=today"
 , Option []    ["until"]             (ReqArg (\d o -> o { optBalanceUntil = Just d }) "DATE")              "compute interest through DATE (YYYY-MM-DD or today);\nexclude transactions with later effective dates"
 , Option []    ["post-on-transactions"] (NoArg (\o -> o { optPostingMode = PostOnTransactions }))                        "post only at transactions (and at --until/--today);\nskip rate-period and automatic year-end postings;\noverrides --post-on-rate-changes"
 , Option []    ["post-on-rate-changes"] (NoArg (\o -> o { optPostingMode = max PostOnRateChanges (optPostingMode o) }))                     "post at transactions and rate-period ends;\nskip automatic year-end postings;\n--post-on-transactions takes precedence;\n--until/--today also post on the final day"
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
 , Option []    ["schedule"]          (ReqArg (setRate "--schedule" scheduled) "SCHEDULE")                "annualized rates and their effective dates;\nrates are fractions: 0.05 means 5% per year\nsyntax: '[(Date1,Rate1),(Date2,Rate2),...]'"
 , Option []    ["schedule-file"]     (ReqArg (\path o -> o { optRate = Right (RateFile path) }) "PATH")   "read a schedule from a text file;\none date and rate per line, starting in column 1;\nrates: 0.05 or 5%; hledger comments allowed"
 , Option []    ["annual-schedule"]   (ReqArg (setRate "--annual-schedule" scheduled) "SCHEDULE")         "alias for --schedule"
 ]

setRate :: Read a => String -> (a -> Rate) -> String -> Options -> Options
setRate flag makeRate value opts = opts { optRate = parsed }
  where
    parsed = case readEither value of
      Left _ -> Left ("invalid value for " ++ flag ++ ": " ++ value ++ "\n")
      Right r -> Right (RateValue (makeRate r))

parseUntil :: String -> IO Day
parseUntil "today" = getCurrentDay
parseUntil date =
  maybe (commandLineError ("invalid value for --until: " ++ date ++
                          "\nexpected YYYY-MM-DD or today\n"))
        pure (parseTimeM True defaultTimeLocale "%Y-%-m-%-d" date)

usageMessage :: String
usageMessage = usageInfo header options
  where header = "Usage: hledger-interest [OPTION...] ACCOUNT\nDefault: post at transactions, rate-period ends, and year ends."

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
  rateInput <- either commandLineError pure (optRate opts)
  cutoff <- traverse parseUntil (optBalanceUntil opts)
  interestAcc <- case args of
                   []    -> commandLineError "required argument ACCOUNT is missing\n"
                   [acc] -> return $ T.pack acc
                   _     -> commandLineError "only one interest ACCOUNT may be specified\n"
  rate <- case rateInput of
    RateValue r -> pure r
    RateFile path -> readScheduleFile path >>=
      either (\msg -> hPutStrLn stderr msg >> exitFailure) (pure . scheduled)
  let ledgerInputOptions = definputopts { balancingopts_ = (balancingopts_ definputopts) { ignore_assertions_ = optIgnoreAssertions opts } }
  inputFiles <- case optInput opts of
                  [] -> pure <$> defaultJournalPath
                  fs -> return (reverse fs)
  jnl <- runExceptT (readJournalFiles ledgerInputOptions inputFiles) >>=
           either (\msg -> hPutStrLn stderr msg >> exitFailure) pure
  let matching = filter (any ((== interestAcc) . paccount) . tpostings) (jtxns jnl)
      ts = sortOn transactionDate2 $
             filter (\t -> maybe True (transactionDate2 t <=) cutoff) matching
      cfg = Config
            { interestAccount = interestAcc
            , sourceAccount = T.pack (optSourceAcc opts)
            , targetAccount = T.pack (optTargetAcc opts)
            , dayCountConvention = dcc
            , interestRate = rate
            , postingMode = optPostingMode opts
            }
  when (null matching) $ commandLineError ("no transactions found for account " ++ T.unpack interestAcc ++ "\n")
  let ts' = runComputer cfg $ do
        mapM_ processTransaction ts
        mapM_ computeInterest cutoff
      result
        | optVerbose opts = ts' ++ ts
        | otherwise       = ts'
  mapM_ (TIO.putStr . showTransaction) (sortOn transactionDate2 result)
