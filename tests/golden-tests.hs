module Main (main) where

import HistoricalRates (historicalModels)

import Control.Monad (forM_, (>=>))

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
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
       ])
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
          invoke ["--post-on-transactions", "--no-year-end-posting"] >>= (@?= expected)
          invoke ["--no-year-end-posting", "--post-on-transactions"] >>= (@?= expected)
      | (convention, flag) <- dayCounts
      ]
  ]

postingModes :: [(String, [String])]
postingModes = [("default", []), ("no-year-end", ["--no-year-end-posting"]),
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
  goldenVsString name ("tests/golden/" ++ name ++ ".golden") $
    successfulOutput (arguments fixture account target args)

successfulOutput :: [String] -> IO B.ByteString
successfulOutput args = do
  (status, out, err) <- runCLI args
  status @?= ExitSuccess
  err @?= B.empty
  pure out

failureCase :: String -> [String] -> String -> TestTree
failureCase name args message = testCase name $ do
  (status, out, err) <- runCLI args
  status @?= ExitFailure 1
  out @?= B.empty
  assertBool ("Expected error: " ++ message ++ "\nActual stderr: " ++ BC.unpack err)
    (BC.pack (message ++ "\n") `B.isPrefixOf` err)

cases :: [(String, String, String, [String])]
cases =
  [ (model ++ "-" ++ convention, "boundaries", principal, [rate, flag, "--quiet"])
  | (model, rate) <- [("constant", "--constant=0.05"), ("annual", "--annual=0.05"),
                      ("schedule", schedule)]
  , (convention, flag) <- dayCounts
  ] ++
  [ ("schedule-no-year-end-" ++ convention, "boundaries", principal,
      [schedule, flag, "--quiet", "--no-year-end-posting"])
  | (convention, flag) <- dayCounts
  ] ++
  [ ("no-year-end-leap-" ++ convention, "leap-crossing", principal,
      ["--annual=0.05", flag, "--quiet", "--no-year-end-posting"])
  | (convention, flag) <- dayCounts
  ] ++
  [ ("rate-change-at-year-end", "leap-crossing", principal,
      ["--schedule=[(2023-01-01,0.05),(2024-01-01,0.07)]", "--act", "--quiet", "--no-year-end-posting"])
  , ("transaction-at-year-end", "year-end-transaction", principal,
      ["--annual=0.05", "--act", "--quiet", "--no-year-end-posting"])
  , ("schedule-transactions-only", "boundaries", principal,
      [schedule, "--act", "--quiet", "--post-on-transactions"])
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
