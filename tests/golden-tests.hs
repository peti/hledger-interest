module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as B
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

-- Argument lists avoid shell quoting and make every invocation reproducible.
main :: IO ()
main = defaultMain $ testGroup "CLI regression"
  [ goldenVsString name ("tests/golden/" ++ name ++ ".golden") $ do
      (status, out, err) <- readProcessWithExitCode "hledger-interest"
        (["--file=tests/fixtures/" ++ fixture ++ ".journal",
          "--source=Expenses:Interest", "--target=" ++ target] ++ args ++
          ["Liabilities:Loan"]) ""
      if status == ExitSuccess && null err
        then pure (B.pack out)
        else fail ("Unexpected exit status or stderr: " ++ show status ++ "\n" ++ err)
  | (name, fixture, target, args) <- cases
  ]

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
