module Main (main) where

import HistoricalRates (historicalModels)

import Control.Monad (forM_, (>=>))

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Time.Calendar (toGregorian)
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)
import System.Exit (ExitCode(..))
import System.Process.Typed (proc, readProcess, setStdin, closed)
import Test.Tasty (defaultMain, localOption, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsString, NoCreateFile(..))
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

main :: IO ()
main = defaultMain $ testGroup "CLI regression"
  [ localOption (NoCreateFile True) $ testGroup "golden"
      ([ goldenCase name fixture "Liabilities:Loan" target args
       | (name, fixture, target, args) <- cases
       ] ++
       [ goldenCase "effective-dates" "effective-dates" "Liabilities:Loan"
           "Liabilities:Interest" ["--annual=0.05", "--act", "--quiet"]
       , goldenCase "literal-account" "literal-account" "Liabilities:Loan(1)"
           "Liabilities:Loan(1)" ["--annual=0.05", "--act"]
       , goldenCase "unicode" "unicode" "Liabilities:Prêt"
           "Liabilities:Prêt" ["--annual=0.05", "--act"]
       ] ++
       [ goldenCase (model ++ "-first-period") (model ++ "-start")
           "Liabilities:Loan" "Liabilities:Interest" (args ++ ["--quiet"])
       | (model, args) <- historicalModels
       ] ++
       [ goldenCase (model ++ "-before-first-rate") "before-coverage"
           "Liabilities:Loan" "Liabilities:Interest" (args ++ ["--quiet"])
       | (model, args) <- historicalModels
       ] ++
       [ goldenCase (model ++ "-after-last-rate") "after-coverage"
           "Liabilities:Loan" "Liabilities:Interest" (args ++ ["--quiet"])
       | (model, args) <- historicalModels
       ] ++
       [ goldenOutput ("schedule-file-" ++ mode ++ "-" ++ convention)
           (baseline ++ convention)
           (fileArguments "schedule" (flag : postingFlags))
       | (mode, postingFlags, baseline) <-
           [("default", [], "schedule-"),
            ("rate-changes", ["--post-on-rate-changes"], "schedule-no-year-end-"),
            ("transactions", ["--post-on-transactions"], "schedule-transactions-only-")]
       , (convention, flag) <- dayCounts
       ] ++
       [ goldenOutput ("schedule-file-" ++ fixture) baseline (fileArguments fixture ["--act"])
       | (fixture, baseline) <-
           [("schedule-crlf", "schedule-act"), ("schedule-no-newline", "schedule-act"),
            ("empty", "zero-rate"), ("comments-only", "zero-rate"),
            ("negative", "negative-rate"), ("positive", "annual-act")]
       ])
  , testGroup "schedule file errors"
      ([ failurePrefix fixture (fileArguments ("schedule-" ++ fixture) ["--act"])
           ("tests/fixtures/schedule-" ++ fixture ++ ".rates:" ++ line ++ ":" ++ column)
       | (fixture, line, column) <-
           [("indented-spaces", "3", "1:"), ("indented-tab", "3", "1:"),
            ("invalid-date", "3", ""), ("mixed-date-separators", "3", ""),
            ("invalid-rate", "3", ""), ("missing-rate", "3", ""),
            ("extra-field", "3", ""), ("inline-hash", "3", ""),
            ("inline-star", "3", ""), ("double-percent", "3", ""),
            ("bad-second-line", "4", "1:")]
       ] ++
       [ failurePrefix "missing-file" (fileArguments "missing" ["--act"])
           "tests/fixtures/missing.rates: "
       ])
  , testGroup "schedule file options"
      [ testCase "last-rate-option-wins" $ do
          let invoke opts = successfulOutput $ arguments "boundaries" "Liabilities:Loan"
                "Liabilities:Loan" (["--act", "--quiet"] ++ opts)
              file name = "--schedule-file=tests/fixtures/" ++ name ++ ".rates"
          fixed <- invoke ["--annual=0.05"]
          scheduled <- invoke [schedule]
          zero <- invoke ["--schedule=[]"]
          invoke [file "missing", "--annual=0.05"] >>= (@?= fixed)
          invoke ["--annual=0.9", file "schedule"] >>= (@?= scheduled)
          invoke [file "schedule", "--schedule=[]"] >>= (@?= zero)
          invoke ["--schedule=bogus", file "schedule"] >>= (@?= scheduled)
          invoke [file "schedule", file "empty"] >>= (@?= zero)
      , testCase "yearless-dates-use-current-year" $ do
          (year, _, _) <- toGregorian . localDay . zonedTimeToLocalTime <$> getZonedTime
          let inline = "--schedule=[(" ++ show year ++ "-02-15,0.05),(" ++ show year ++ "-07-01,0.07)]"
          expected <- successfulOutput (arguments "boundaries" "Liabilities:Loan"
            "Liabilities:Loan" [inline, "--act", "--quiet"])
          successfulOutput (fileArguments "yearless" ["--act"]) >>= (@?= expected)
      ]
  , testGroup "errors"
      ([ failureCase ("removed-" ++ model) ["--" ++ model]
           ("unrecognized option `--" ++ model ++ "'")
       | (model, _) <- historicalModels
       ] ++
       [ failureCase ("invalid-" ++ flag)
           (arguments "boundaries" "Liabilities:Loan" "Liabilities:Interest"
             ["--act", "--" ++ flag ++ "=bogus"])
           ("invalid value for --" ++ flag ++ ": bogus")
       | flag <- ["constant", "annual", "schedule", "annual-schedule"]
       ] ++
       [ failureCase "abbreviated-account"
           (arguments "boundaries" "Loan" "Liabilities:Interest" ["--annual=0.05", "--act"])
           "no transactions found for account Loan"
       , failureCase "missing-account-before-reading-journal"
           ["--file=tests/fixtures/does-not-exist.journal", "--source=Expenses:Interest",
            "--target=Liabilities:Interest", "--act", "--annual=0.05"]
           "required argument ACCOUNT is missing"
       , failureCase "invalid-rate-before-reading-journal"
           ["--file=tests/fixtures/does-not-exist.journal", "--source=Expenses:Interest",
            "--target=Liabilities:Interest", "--act", "--annual=bogus", "Liabilities:Loan"]
           "invalid value for --annual: bogus"
       ])
  , testGroup "rate aliases"
      [ testCase (mode ++ "-" ++ convention) $ do
          let invoke rate = successfulOutput $ arguments "boundaries" "Liabilities:Loan"
                "Liabilities:Loan" ([rate, flag, "--quiet"] ++ postingFlags)
              rates = "[(2023-01-01,0.05)]"
          expected <- invoke ("--schedule=" ++ rates)
          forM_ ["--annual-schedule=" ++ rates, "--constant=0.05", "--annual=0.05"]
            (invoke >=> (@?= expected))
      | (mode, postingFlags) <- postingModes
      , (convention, flag) <- dayCounts
      ]
  , testGroup "posting flag precedence"
      [ testCase convention $ do
          let invoke flags = successfulOutput $ arguments "boundaries" "Liabilities:Loan"
                "Liabilities:Loan" ([schedule, flag, "--quiet"] ++ flags)
          expected <- invoke ["--post-on-transactions"]
          invoke ["--post-on-transactions", "--post-on-rate-changes"] >>= (@?= expected)
          invoke ["--post-on-rate-changes", "--post-on-transactions"] >>= (@?= expected)
      | (convention, flag) <- dayCounts
      ]
  , testGroup "cutoff options"
      [ testCase "last-cutoff-option-wins-and-today-is-an-alias" $ do
          let invoke flags = successfulOutput $ arguments "boundaries" "Liabilities:Loan"
                "Liabilities:Loan" ([schedule, "--act", "--quiet"] ++ flags)
          cutoff <- invoke ["--until=2024-01-10"]
          invoke ["--today", "--until=2024-01-10"] >>= (@?= cutoff)
          invoke ["--until=bad", "--until=2024-01-10"] >>= (@?= cutoff)
          full <- invoke []
          invoke ["--until=2024-01-10", "--until=2024-03-31"] >>= (@?= full)
          today <- invoke ["--until=today"]
          invoke ["--today"] >>= (@?= today)
          invoke ["--until=2024-01-10", "--today"] >>= (@?= today)
      , testCase "unpadded-date" $ do
          let invoke date = successfulOutput $ arguments "leap-crossing" "Liabilities:Loan"
                "Liabilities:Loan" ["--annual=0.05", "--act", "--quiet", "--until=" ++ date]
          expected <- invoke "2024-01-05"
          invoke "2024-1-5" >>= (@?= expected)
      , testCase "cutoff-before-first-transaction" $
          forM_ postingModes $ \(_, flags) -> do
            out <- successfulOutput $ arguments "boundaries" "Liabilities:Loan"
              "Liabilities:Loan" ([schedule, "--act", "--until=2023-01-30"] ++ flags)
            out @?= B.empty
      ]
  , testGroup "cutoff errors"
      ([ failureCase ("invalid-" ++ date)
           (arguments "boundaries" "Liabilities:Loan" "Liabilities:Loan"
             ["--annual=0.05", "--act", "--until=" ++ date])
           ("invalid value for --until: " ++ date)
       | date <- ["bad", "2023-02-29", "2024-13-01", "2024/01/10", "2024-01-10junk", ""]
       ] ++
       [ failureCase "invalid-cutoff-before-reading-journal"
           ["--file=tests/fixtures/does-not-exist.journal", "--source=Expenses:Interest",
            "--target=Liabilities:Loan", "--annual=0.05", "--act", "--until=bad", "Liabilities:Loan"]
           "invalid value for --until: bad"
       ])
  ]

postingModes :: [(String, [String])]
postingModes = [("default", []), ("rate-changes", ["--post-on-rate-changes"]),
                ("transactions", ["--post-on-transactions"])]

dayCounts :: [(String, String)]
dayCounts = [("act", "--act"), ("30-360", "--30-360"),
             ("30E-360", "--30E-360"), ("30E-360isda", "--30E-360isda")]

schedule :: String
schedule = "--schedule=[(2023-02-15,0.05),(2023-07-01,0.07),(2024-02-29,0.03)]"

-- Capture bytes directly, preserving Unicode and avoiding locale-dependent decoding.
runCLI :: [String] -> IO (ExitCode, B.ByteString, B.ByteString)
runCLI = readProcess . setStdin closed . proc "hledger-interest"

arguments :: String -> String -> String -> [String] -> [String]
arguments fixture account target args =
  ["--file=tests/fixtures/" ++ fixture ++ ".journal",
   "--source=Expenses:Interest", "--target=" ++ target] ++ args ++ [account]

goldenCase :: String -> String -> String -> String -> [String] -> TestTree
goldenCase name fixture account target args =
  goldenOutput name name (arguments fixture account target args)

goldenOutput :: String -> String -> [String] -> TestTree
goldenOutput name baseline args =
  goldenVsString name ("tests/golden/" ++ baseline ++ ".golden") (successfulOutput args)

fileArguments :: String -> [String] -> [String]
fileArguments fixture flags = arguments "boundaries" "Liabilities:Loan" "Liabilities:Loan"
  (["--schedule-file=tests/fixtures/" ++ fixture ++ ".rates", "--quiet"] ++ flags)

successfulOutput :: [String] -> IO B.ByteString
successfulOutput args = do
  (status, out, err) <- runCLI args
  status @?= ExitSuccess
  err @?= B.empty
  pure out

failureCase :: String -> [String] -> String -> TestTree
failureCase name args message = failurePrefix name args (message ++ "\n")

failurePrefix :: String -> [String] -> String -> TestTree
failurePrefix name args message = testCase name $ do
  (status, out, err) <- runCLI args
  status @?= ExitFailure 1
  out @?= B.empty
  assertBool ("Expected error: " ++ message ++ "\nActual stderr: " ++ BC.unpack err)
    (BC.pack message `B.isPrefixOf` err)

cases :: [(String, String, String, [String])]
cases =
  [ (model ++ "-" ++ convention, "boundaries", principal, [rate, flag, "--quiet"])
  | (model, rate) <- [("constant", "--constant=0.05"), ("annual", "--annual=0.05"),
                      ("schedule", schedule)]
  , (convention, flag) <- dayCounts
  ] ++
  [ ("schedule-no-year-end-" ++ convention, "boundaries", principal,
      [schedule, flag, "--quiet", "--post-on-rate-changes"])
  | (convention, flag) <- dayCounts
  ] ++
  [ ("no-year-end-leap-" ++ convention, "leap-crossing", principal,
      ["--annual=0.05", flag, "--quiet", "--post-on-rate-changes"])
  | (convention, flag) <- dayCounts
  ] ++
  [ ("schedule-transactions-only-" ++ convention, "boundaries", principal,
      [schedule, flag, "--quiet", "--post-on-transactions"])
  | (convention, flag) <- dayCounts
  ] ++
  [ ("until-" ++ mode ++ "-" ++ convention, "boundaries", principal,
      [schedule, flag, "--quiet", "--until=2024-01-10"] ++ postingFlags)
  | (mode, postingFlags) <- postingModes
  , (convention, flag) <- dayCounts
  ] ++
  [ ("until-after-last-transaction", "leap-crossing", principal,
      ["--annual=0.05", "--act", "--quiet", "--until=2024-01-05"])
  , ("until-effective-dates", "cutoff", principal,
      ["--annual=0.05", "--act", "--until=2024-01-01"])
  ] ++
  [ ("help", "boundaries", principal, ["--help"])
  , ("rate-change-at-year-end", "leap-crossing", principal,
      ["--schedule=[(2023-01-01,0.05),(2024-01-01,0.07)]", "--act", "--quiet", "--post-on-rate-changes"])
  , ("transaction-at-year-end", "year-end-transaction", principal,
      ["--annual=0.05", "--act", "--quiet", "--post-on-rate-changes"])
  , ("annual-separate-account", "boundaries", "Liabilities:Interest",
      ["--annual=0.05", "--act", "--quiet"])
  , ("annual-verbose", "boundaries", principal, ["--annual=0.05", "--act"])
  , ("zero-rate", "boundaries", principal, ["--annual=0", "--act", "--quiet"])
  , ("negative-rate", "boundaries", principal, ["--annual=-0.01", "--act", "--quiet"])
  ] ++
  [ (model, if model == "bgb288" then "boundaries" else "historical", principal,
      args ++ ["--quiet"])
  | (model, args) <- historicalModels
  ]
  where
    principal = "Liabilities:Loan"
