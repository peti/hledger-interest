{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (zipWithM_)
import Data.Decimal (Decimal)
import Data.Ratio ((%))
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)
import Hledger.Interest
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "Independent calculations"
  [ dayCountTests
  , balanceTests
  , testGroup "Accrual"
      [ testCase "repayment changes the next period, not the payment day" $ do
          -- 3650 at 10% earns 1 per day in 2023. After repaying 1000,
          -- ten more days earn 2650 * 10% * 10 / 365 = 530/73.
          let cfg = baseConfig
          assertInterest cfg (compute cfg [(day 2023 1 1, 3650), (day 2023 1 11, -1000)] (day 2023 1 21))
            [(day 2023 1 11, 10), (day 2023 1 21, 530 % 73)]
      , testCase "same-day repayments do not accrue another day" $ do
          -- Both repayments leave 2000; the next ten days earn 400/73.
          let cfg = baseConfig
          assertInterest cfg (compute cfg
            [(day 2023 1 1, 3650), (day 2023 1 11, -1000), (day 2023 1 11, -650)] (day 2023 1 21))
            [(day 2023 1 11, 10), (day 2023 1 21, 400 % 73)]
      , testCase "February 29th is included with a 366-day denominator" $ do
          -- The loan day is excluded; February 29th and March 1st each
          -- earn 3660 * 10% / 366 = 1.
          let cfg = baseConfig
          assertInterest cfg (compute cfg [(day 2024 2 28, 3660)] (day 2024 3 1))
            [(day 2024 3 1, 2)]
      , testCase "year-end postings compound only in the principal account" $ do
          -- December 31st earns 1. The next two days earn
          -- 3651 * 10% * 2 / 366 = 1217/610 after compounding.
          let cfg = baseConfig { targetAccount = "Assets:Loan" }
          assertInterest cfg (compute cfg [(day 2023 12 30, 3650)] (day 2024 1 2))
            [(day 2023 12 31, 1), (day 2024 1 2, 1217 % 610)]
      , testCase "a separate interest account does not compound" $ do
          -- The same two leap-year days use the original principal:
          -- 3650 * 10% * 2 / 366 = 365/183.
          let cfg = baseConfig
          assertInterest cfg (compute cfg [(day 2023 12 30, 3650)] (day 2024 1 2))
            [(day 2023 12 31, 1), (day 2024 1 2, 365 % 183)]
      ]
  , testGroup "Internal year splits"
      [ testCase name $ do
          -- No intermediate posting: 1 non-leap-year day plus 2 leap-year
          -- days earn 1 + 365/183 = 548/183 on unchanged principal.
          let cfg = baseConfig { targetAccount = "Assets:Loan"
                               , postingMode = mode }
          assertInterest cfg (compute cfg [(day 2023 12 30, 3650)] (day 2024 1 2))
            [(day 2024 1 2, 548 % 183)]
      | (name, mode) <-
          [("rate-change posting", PostOnRateChanges), ("transaction-only posting", PostOnTransactions)]
      ]
  , testGroup "Integrated day counts"
      [ testCase name $ do
          -- Accrual starts February 28th, after the February 27th loan.
          -- At 3600 and 10%, every synthetic 360-day-convention day earns 1.
          -- Actual/Actual uses 32 actual days: 360 * 32 / 365 = 2304/73.
          let cfg = baseConfig { dayCountConvention = convention }
          assertInterest cfg (compute cfg [(day 2023 2 27, 3600)] (day 2023 3 31))
            [(day 2023 3 31, expected)]
      | (name, convention, expected) <-
          [("actual", diffAct, 2304 % 73), ("raw 30/360", diff30_360, 34),
           ("30E/360", diff30E_360, 33), ("30E/360 ISDA", diff30E_360isda, 33)]
      ]
  , testGroup "Periods start after an adjusted day"
      [ testCase name $ do
          -- Accrual starts January 31st, which 30E/360 counts as the 30th,
          -- so the period must be counted from the loan day (issue #29).
          -- Actual/Actual uses 29 actual days: 360 * 29 / 365 = 2088/73.
          let cfg = baseConfig { dayCountConvention = convention }
          assertInterest cfg (compute cfg [(day 2023 1 30, 3600)] (day 2023 2 28))
            [(day 2023 2 28, expected)]
      | (name, convention, expected) <-
          [("actual", diffAct, 2088 % 73), ("raw 30/360", diff30_360, 28),
           ("30E/360", diff30E_360, 28), ("30E/360 ISDA", diff30E_360isda, 30)]
      ]
  , testGroup "Monthly periods add up to a 360-day year"
      [ testCase name $ do
          -- Payments on the 30th (February 28th) split a year into twelve
          -- periods; at 1 per synthetic day they must earn exactly 360.
          let cfg = baseConfig { dayCountConvention = convention }
              payments = [(day 2023 m (if m == 2 then 28 else 30), 0) | m <- [1 .. 12]]
              txns = compute cfg ((day 2022 12 30, 3600) : payments) (day 2023 12 30)
          sum [aquantity a | t <- txns, p <- tpostings t, paccount p == targetAccount cfg,
                             a <- amounts (pamount p)] @?= 360
      | (name, convention) <-
          [("raw 30/360", diff30_360), ("30E/360", diff30E_360), ("30E/360 ISDA", diff30E_360isda)]
      ]
  , testGroup "Rate changes"
      [ testCase "new rate applies on its effective date" $ do
          -- January 2nd-10th: 9 days at 1/day; January 11th-20th:
          -- 10 days at 2/day. A separate account keeps principal at 3600.
          let cfg = rateChangeConfig
          assertInterest cfg (compute cfg [(day 2023 1 1, 3600)] (day 2023 1 20))
            [(day 2023 1 10, 9), (day 2023 1 20, 20)]
      , testCase "posted interest compounds at a rate change" $ do
          -- The second period uses 3609: 3609 * 20% * 10 / 360 = 20.05.
          let cfg = rateChangeConfig { targetAccount = "Assets:Loan" }
          assertInterest cfg (compute cfg [(day 2023 1 1, 3600)] (day 2023 1 20))
            [(day 2023 1 10, 9), (day 2023 1 20, 20.05)]
      , testCase "deferred interest combines rates without compounding" $ do
          let cfg = rateChangeConfig { targetAccount = "Assets:Loan", postingMode = PostOnTransactions }
          assertInterest cfg (compute cfg [(day 2023 1 1, 3600)] (day 2023 1 20))
            [(day 2023 1 20, 29)]
      , testCase "dates before the first rate earn zero" $ do
          let cfg = baseConfig { dayCountConvention = diff30E_360
                               , interestRate = scheduled [(day 2023 1 11, 0.1)] }
          assertInterest cfg (compute cfg [(day 2023 1 1, 3600)] (day 2023 1 20))
            [(day 2023 1 10, 0), (day 2023 1 20, 10)]
      ]
  , testGroup "Signs"
      [ testCase name $ do
          -- Ten 360-day-convention days on +/-3600 at +/-10%.
          let cfg = baseConfig { dayCountConvention = diff30E_360, interestRate = constant rate }
          assertInterest cfg (compute cfg [(day 2023 1 1, principal)] (day 2023 1 11))
            [(day 2023 1 11, expected)]
      | (name, principal, rate, expected) <-
          [("negative principal", -3600, 0.1, -10),
           ("negative rate", 3600, -0.1, -10),
           ("negative principal and rate", -3600, -0.1, 10)]
      ]
  , testGroup "Rounding policy"
      [ testCase "sub-cent quantities are posted and compounded unrounded" $ do
          -- 100 * 1.44% / 360 = 0.004. After posting it, the next day
          -- earns 100.004 * 1.44% / 360 = 0.00400016, not 0.004.
          let cfg = baseConfig { dayCountConvention = diff30E_360
                               , interestRate = constant 0.0144
                               , targetAccount = "Assets:Loan" }
              txns = compute cfg [(day 2023 1 1, 100), (day 2023 1 2, 0)] (day 2023 1 3)
          assertInterest cfg txns [(day 2023 1 2, 0.004), (day 2023 1 3, 0.00400016)]
          assertBool "Both sub-cent postings should display zero to two decimal places"
            (all (T.isInfixOf "EUR 0.00\n" . showTransaction) txns)
      , testCase "deferred accrual is summed before display rounding" $ do
          -- Repeat the rate on January 3rd to force two calculation segments.
          -- Each earns 0.004 on unchanged principal; their sum is 0.008,
          -- which displays as 0.01 rather than two separately rounded zeros.
          let cfg = baseConfig { dayCountConvention = diff30E_360
                               , interestRate = scheduled [(day 2023 1 1, 0.0144), (day 2023 1 3, 0.0144)]
                               , targetAccount = "Assets:Loan", postingMode = PostOnTransactions }
              txns = compute cfg [(day 2023 1 1, 100)] (day 2023 1 3)
          assertInterest cfg txns [(day 2023 1 3, 0.008)]
          assertBool "The combined posting should display EUR 0.01"
            (any (T.isInfixOf "EUR 0.01\n" . showTransaction) txns)
      ]
  ]

dayCountTests :: TestTree
dayCountTests = testGroup "Day-count definitions"
  [ testCase (name ++ ": " ++ label) $ convention from to @?= expected
  | (label, from, to, counts) <-
      [("non-leap February end", day 2023 2 28, day 2023 3 31, [31,33,32,30]),
       ("leap February end", day 2024 2 29, day 2024 3 31, [31,32,31,30]),
       ("start on the 31st", day 2023 1 31, day 2023 2 15, [15,14,15,15]),
       ("end in February", day 2023 1 15, day 2023 2 28, [44,43,43,45]),
       ("cross December 31st", day 2023 12 31, day 2024 1 1, [1,0,1,1])]
  , ((name, convention), expected) <- zip conventions counts
  ]
  where
    conventions = [("actual", diffAct), ("raw 30/360", diff30_360),
                   ("30E/360", diff30E_360), ("30E/360 ISDA", diff30E_360isda)]

baseConfig :: Config
baseConfig = Config
  { interestAccount = "Assets:Loan"
  , sourceAccount = "Income:Interest"
  , targetAccount = "Assets:Interest"
  , dayCountConvention = diffAct
  , interestRate = constant 0.1
  , postingMode = PostAtAllBoundaries
  }

rateChangeConfig :: Config
rateChangeConfig = baseConfig
  { dayCountConvention = diff30E_360
  , interestRate = scheduled [(day 2023 1 1, 0.1), (day 2023 1 11, 0.2)]
  }

day :: Integer -> Int -> Int -> Day
day = fromGregorian

compute :: Config -> [(Day, Decimal)] -> Day -> [Transaction]
compute cfg changes cutoff = runComputer cfg $ do
  mapM_ (processTransaction . uncurry principalChange) changes
  computeInterest cutoff

principalChange :: Day -> Decimal -> Transaction
principalChange date quantity = nulltransaction
  { tdate = date
  , tpostings = [mkPosting "Assets:Loan" quantity, mkPosting "Equity:Funding" (negate quantity)]
  }

mkPosting :: AccountName -> Decimal -> Posting
mkPosting = currencyPosting "EUR"

currencyPosting :: CommoditySymbol -> AccountName -> Decimal -> Posting
currencyPosting currency account q = nullposting
  { paccount = account
  , pamount = mixed [nullamt { acommodity = currency, aquantity = q
                            , astyle = amountstyle { asprecision = Precision 2, ascommodityspaced = True } }]
  }

-- Expected values are hand-derived rational numbers, not output captured from
-- the implementation. Compare internal quantities before any display rounding.
assertInterest :: Config -> [Transaction] -> [(Day, Rational)] -> Assertion
assertInterest cfg txns expected = do
  let actual = [(tdate t, aquantity a) | t <- txns, p <- tpostings t,
                paccount p == targetAccount cfg, a <- amounts (pamount p)]
  map fst actual @?= map fst expected
  zipWithM_ assertQuantity (map snd expected) (map snd actual)
  where
    -- Decimal division is finite precision. This tolerance is far below a
    -- monetary rounding unit, while allowing recurring rational fractions.
    assertQuantity rational quantity = assertBool
      ("Expected " ++ show rational ++ "; got " ++ show quantity)
      (abs (toRational quantity - rational) < 1 % (10 ^ (30 :: Int)))

balanceTests :: TestTree
balanceTests = testGroup "Balance transitions"
  [ testGroup name
      [ testCase "full repayment and a new loan" $ do
          -- First ten days earn 10. The balance is then zero across a rate
          -- change. Ten synthetic days on the new 7200 at 20% earn 40.
          let cfg = plain { interestRate = scheduled [(day 2023 1 1, 0.1), (day 2023 1 16, 0.2)] }
          assertInterest cfg (compute cfg
            [(day 2023 1 1, 3600), (day 2023 1 11, -3600), (day 2023 1 21, 7200)] cutoff)
            [(day 2023 1 11, 10), (cutoff, 40)]
      , testCase "repayment clears compounded interest too" $ do
          let cfg = compound { interestRate = scheduled [(day 2023 1 1, 0.1), (day 2023 1 16, 0.2)] }
          assertInterest cfg (compute cfg
            [(day 2023 1 1, 3600), (day 2023 1 11, -3610), (day 2023 1 21, 7200)] cutoff)
            [(day 2023 1 11, 10), (cutoff, 40)]
      , testCase "overpayment reverses interest" $
          assertInterest plain (compute plain
            [(day 2023 1 1, 3600), (day 2023 1 11, -7200)] cutoff)
            [(day 2023 1 11, 10), (cutoff, -20)]
      , testCase "overpayment retains unrounded compounded balance" $
          -- After posting 10 and repaying 7200, -3590 earns -3590/180.
          assertInterest compound (compute compound
            [(day 2023 1 1, 3600), (day 2023 1 11, -7200)] cutoff)
            [(day 2023 1 11, 10), (cutoff, (-359) % 18)]
      , testCase "all selected-account postings contribute" $ do
          -- Net opening principal 1800 earns 5; the net repayment of 900
          -- leaves 900, earning another 5 over twenty synthetic days.
          let balanceChange date quantities = nulltransaction
                { tdate = date, tpostings = map (mkPosting "Assets:Loan") quantities ++
                    [mkPosting "Equity:Funding" (negate (sum quantities))] }
              txns = runComputer plain $ do
                processTransaction (balanceChange (day 2023 1 1) [3600, -1800])
                processTransaction (balanceChange (day 2023 1 11) [-400, -500])
                computeInterest cutoff
          assertInterest plain txns [(day 2023 1 11, 5), (cutoff, 5)]
      , testCase "currencies accrue independently" $ do
          -- EUR: ten days on 3600, then twenty on 2400 = 10 + 40/3.
          -- USD: ten days on 7200, then twenty on 3600 = 20 + 20.
          let balanceChange date euroAmount dollarAmount = nulltransaction
                { tdate = date, tpostings =
                    [currencyPosting currency account quantity
                    | (currency, q) <- [("EUR", euroAmount), ("USD", dollarAmount)]
                    , (account, quantity) <- [("Assets:Loan", q), ("Equity:Funding", negate q)]] }
              txns = runComputer plain $ do
                processTransaction (balanceChange (day 2023 1 1) 3600 7200)
                processTransaction (balanceChange (day 2023 1 11) (-1200) (-3600))
                computeInterest cutoff
              currencyInterest currency =
                [t { tpostings = [p { pamount = mixed (filter ((== currency) . acommodity) (amounts (pamount p))) }
                                 | p <- tpostings t] } | t <- txns]
          assertInterest plain (currencyInterest "EUR") [(day 2023 1 11, 10), (cutoff, 40 % 3)]
          assertInterest plain (currencyInterest "USD") [(day 2023 1 11, 20), (cutoff, 20)]
      ]
  | (name, mode) <- [("default", PostAtAllBoundaries), ("rate changes", PostOnRateChanges),
                     ("transactions", PostOnTransactions)]
  , let plain = baseConfig { dayCountConvention = diff30E_360, postingMode = mode }
        compound = plain { targetAccount = "Assets:Loan" }
  ]
  where cutoff = day 2023 2 1
