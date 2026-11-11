module Main (main) where

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
           "Liabilities:Loan" "Liabilities:Interest" [flag, "--quiet"]
       | (model, flag) <- bankModels
       ])
  , testGroup "errors"
      ([ failureCase (model ++ "-before-coverage")
           (arguments "before-coverage" "Liabilities:Loan" "Liabilities:Interest" [flag, "--quiet"])
           ("hledger-interest: no " ++ label ++ " is known for 2000-01-02")
       | (model, flag, label) <-
           [("ing-diba", "--ing-diba", "ING-DiBa interest rate"),
            ("db24", "--db24", "Deutsche Bank 24 interest rate"),
            ("bgb288", "--bgb288", "BGB base rate")]
       ] ++
       [ failureCase ("invalid-" ++ flag)
           (arguments "boundaries" "Liabilities:Loan" "Liabilities:Interest"
             ["--act", "--" ++ flag ++ "=bogus"])
           ("invalid value for --" ++ flag ++ ": bogus")
       | flag <- ["constant", "annual", "annual-schedule"]
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
  ]

bankModels :: [(String, String)]
bankModels = [("bgb288", "--bgb288"), ("db24", "--db24"), ("ing-diba", "--ing-diba")]

-- Capture bytes directly, preserving Unicode and avoiding locale-dependent decoding.
runCLI :: [String] -> IO (ExitCode, B.ByteString, B.ByteString)
runCLI = readProcess . setStdin closed . proc "hledger-interest"

arguments :: String -> String -> String -> [String] -> [String]
arguments fixture account target args =
  ["--file=tests/fixtures/" ++ fixture ++ ".journal",
   "--source=Expenses:Interest", "--target=" ++ target] ++ args ++ [account]

goldenCase :: String -> String -> String -> String -> [String] -> TestTree
goldenCase name fixture account target args =
  goldenVsString name ("tests/golden/" ++ name ++ ".golden") $ do
    (status, out, err) <- runCLI (arguments fixture account target args)
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
  , (convention, flag) <- [("act", "--act"), ("30-360", "--30-360"),
                           ("30E-360", "--30E-360"), ("30E-360isda", "--30E-360isda")]
  ] ++
  [ ("schedule-transactions-only", "boundaries", principal,
      [schedule, "--act", "--quiet", "--post-on-transactions"])
  , ("annual-separate-account", "boundaries", "Liabilities:Interest",
      ["--annual=0.05", "--act", "--quiet"])
  , ("annual-verbose", "boundaries", principal, ["--annual=0.05", "--act"])
  , ("zero-rate", "boundaries", principal, ["--annual=0", "--act", "--quiet"])
  , ("negative-rate", "boundaries", principal, ["--annual=-0.01", "--act", "--quiet"])
  , ("bgb288", "boundaries", principal, ["--bgb288", "--quiet"])
  , ("db24", "historical", principal, ["--db24", "--quiet"])
  , ("ing-diba", "historical", principal, ["--ing-diba", "--quiet"])
  ]
  where
    principal = "Liabilities:Loan"
    schedule = "--annual-schedule=[(2023-02-15,0.05),(2023-07-01,0.07),(2024-02-29,0.03)]"
