module Hledger.Interest
  ( Computer, runComputer
  , Config(..), PostingMode(..)
  , InterestState(..), nullInterestState
  , processTransaction, processTransactionsUntil, computeInterest
  , module Hledger.Interest.DayCountConvention
  , module Hledger.Interest.Rate
  , module Hledger.Data
  )
  where

import Hledger.Data
import Hledger.Interest.DayCountConvention
import Hledger.Interest.Rate

import Control.Monad
import Control.Monad.RWS
import Data.Decimal
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

type Computer = RWS Config [Transaction] InterestState

runComputer :: Config -> Computer () -> [Transaction]
runComputer cfg f = ts
  where ((),_,ts) = runRWS f cfg nullInterestState

-- | Increasingly restrictive posting policies. This ordering lets the CLI
-- combine flags with 'max', so transaction-only posting always takes precedence.
data PostingMode = PostAtAllBoundaries | PostOnRateChanges | PostOnTransactions
  deriving (Eq, Ord, Show)

data Config = Config
  { interestAccount :: AccountName
  , sourceAccount :: AccountName
  , targetAccount :: AccountName
  , dayCountConvention :: DayCountConvention
  , interestRate :: Rate
  , postingMode :: PostingMode
    -- ^ Select automatic posting boundaries. Transactions and an explicit
    -- cutoff always post; actual-day calculations split at year ends in all modes.
  }

data InterestState = InterestState
  { balancedUntil :: Day
  , balance :: MixedAmount
  , accruedDays :: Integer          -- ^ interest accrued but not posted yet
  , accruedInterest :: MixedAmount
  }

nullInterestState :: InterestState
nullInterestState = InterestState
  { balancedUntil = nulldate
  , balance = nullmixedamt
  , accruedDays = 0
  , accruedInterest = nullmixedamt
  }

processTransaction :: Transaction -> Computer ()
processTransaction ts = processTransactionsUntil Nothing [ts]

-- | Apply matching postings in posting-date order, optionally ignoring
-- changes after the cutoff date.
processTransactionsUntil :: Maybe Day -> [Transaction] -> Computer ()
processTransactionsUntil cutoff ts = do
  interestAcc <- asks interestAccount
  let changes = sortOn (postingDate2 . snd)
        [ (t, p) | t <- ts, p <- tpostings t, interestAcc == paccount p
                 , maybe True (postingDate2 p <=) cutoff ]
  forM_ changes $ \(t, p) -> do
    let date = postingDate2 p
        principal = mapMixedAmount (\a -> a { acost = Nothing, acostbasis = Nothing }) (pamount p)
    computeInterest date
    modify (\st -> st { balance = balance st + principal })

-- | Compute and post all interest up to and including the given day.
computeInterest :: Day -> Computer ()
computeInterest day = accrueInterest day >> postInterest day Nothing

accrueInterest :: Day -> Computer ()
accrueInterest day = do
  from <- gets balancedUntil
  bal <- gets balance
  when (day < pred from) $
    error "hledger-interest: cannot compute interest backwards; transactions must be ordered by effective date"
  -- A second transaction on the same day must not rewind the accrual cursor.
  when (day >= from) $
    if mixedAmountIsZero bal
      -- No rate is needed before the first loan or while its balance is zero.
      then modify (\st -> st { balancedUntil = succ day })
      else do
        rate <- asks interestRate
        mode <- asks postingMode
        yearDays <- daysInYear from
        let (endOfPeriod,ratePerAnno) = rate from
            endOfYear = yearEnd from
            -- Fixed 360-day conventions need no internal year split. Avoiding
            -- one also preserves their day counts across December 31st.
            yearLimit
              | mode == PostAtAllBoundaries || yearDays /= 360 = endOfYear
              | otherwise = day
            to = minimum [day, endOfPeriod, yearLimit]
            newFrom = succ to
        when (to < from) $
          error "hledger-interest: interest rate period ends before its start"
        modify (\st -> st { balancedUntil = newFrom })
        diff <- asks dayCountConvention
        -- Count from the last accounted day, not the first accrual day: the
        -- 30/360 conventions adjust some dates, so only this form makes the
        -- counts of consecutive periods add up to the count over the span.
        let days = pred from `diff` to
            interest = mapMixedAmount (\a -> a { aquantity = (aquantity a * ratePerAnno) / fromInteger yearDays * fromInteger days }) bal
        modify (\st -> st { accruedDays = accruedDays st + days
                          , accruedInterest = accruedInterest st + interest
                          })
        -- Internal calendar-year splits do not change the principal unless
        -- the posting policy calls for a posting at that boundary.
        let shouldPost = case mode of
              PostAtAllBoundaries -> True
              PostOnRateChanges -> to == day || to == endOfPeriod
              PostOnTransactions -> False
        when shouldPost (postInterest to (Just ratePerAnno))
        when (newFrom <= day) (accrueInterest day)

-- | Post all accrued interest as one transaction on the given day. The
-- description mentions the rate only if one is given, i.e. if the interest
-- was accrued over a single rate period.
postInterest :: Day -> Maybe Decimal -> Computer ()
postInterest day ratePerAnno = do
  st <- get
  when (accruedDays st > 0) $ do
    put st { accruedDays = 0, accruedInterest = nullmixedamt }
    t <- mkTrans day (accruedDays st) (accruedInterest st) ratePerAnno
    tell [t]
    processTransaction t

daysInYear :: Day -> Computer Integer
daysInYear now = asks dayCountConvention >>= \diff -> return (day1 `diff` day2)
  where day1 = fromGregorian (fst (toOrdinalDate now)) 1 1
        day2 = fromGregorian (succ (fst (toOrdinalDate now))) 1 1

yearEnd :: Day -> Day
yearEnd date = fromGregorian (fst (toOrdinalDate date)) 12 31

-- | Create the transaction for the given interest, which was accrued over
-- the given number of days.
mkTrans :: Day -> Integer -> MixedAmount -> Maybe Decimal -> Computer Transaction
mkTrans day days interest ratePerAnno = do
  bal <- gets balance
  srcAcc <- asks sourceAccount
  targetAcc <- asks targetAccount
  let t = nulltransaction
          { tdate          = day
          , tdescription   = T.pack $ maybe "" (\r -> showPercent r ++ " ") ratePerAnno ++ "interest for " ++ showMixedAmountOneLine bal ++ " over " ++ show days ++ " days"
          , tpostings      = [pTarget,pSource]
          }
      -- balance might have been an exact amount, in which case its precision might
      -- be set to 0 decimal places. We ensure that interest is displayed with at least
      -- two decimal places. This changes rendering only: the full computed
      -- quantity is posted and, when applicable, compounded (see README).
      pTarget = nullposting
          { paccount       = targetAcc
          , pamount        = mapMixedAmount (amountSetPrecisionMin 2) interest
          , ptype          = RegularPosting
          , ptransaction   = Just t
          }
      pSource = nullposting
          { paccount       = srcAcc
          , pamount        = negate (pamount pTarget)
          , ptype          = RegularPosting
          , ptransaction   = Just t
          }
  return t

showPercent :: Decimal -> String
showPercent r = shows (r * 100) "%"
