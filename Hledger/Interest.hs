module Hledger.Interest
  ( Computer, runComputer
  , Config(..)
  , InterestState(..), nullInterestState
  , processTransaction, computeInterest
  , module Hledger.Interest.DayCountConvention
  , module Hledger.Interest.Rate
  , module Hledger.Data
  )
  where

import Hledger.Data
import Hledger.Interest.DayCountConvention
import Hledger.Interest.Rate

import Control.Monad.RWS
import Data.Decimal
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

type Computer = RWS Config [Transaction] InterestState

runComputer :: Config -> Computer () -> [Transaction]
runComputer cfg f = ts
  where ((),_,ts) = runRWS f cfg nullInterestState

data Config = Config
  { interestAccount :: AccountName
  , sourceAccount :: AccountName
  , targetAccount :: AccountName
  , dayCountConvention :: DayCountConvention
  , interestRate :: Rate
  }

data InterestState = InterestState
  { balancedUntil :: Day
  , balance :: MixedAmount
  }

nullInterestState :: InterestState
nullInterestState = InterestState
  { balancedUntil = nulldate
  , balance = nullmixedamt
  }

processTransaction :: Transaction -> Computer ()
processTransaction ts = do
  let day = fromMaybe (tdate ts) (tdate2 ts)
  computeInterest day
  interestAcc <- asks interestAccount
  let posts = [ p | p <- tpostings ts, interestAcc == paccount p ]
  forM_ posts $ \p -> do
    bal <- gets balance
    modify (\st -> st { balance = bal + pamount p })

computeInterest :: Day -> Computer ()
computeInterest day = do
  from <- gets balancedUntil
  bal <- gets balance
  rate <- asks interestRate
  let (endOfPeriod,ratePerAnno) = rate from
      to = min day endOfPeriod
      newFrom = succ to
  modify (\st -> st { balancedUntil = newFrom })
  when (to >= from && not (mixedAmountIsZero bal)) $ do
    diff <- asks dayCountConvention
    t <- mkTrans to ((from `diff` to) + 1) ratePerAnno
    tell [t]
    processTransaction t
  when (newFrom < day) (computeInterest day)

daysInYear :: Day -> Computer Integer
daysInYear now = asks dayCountConvention >>= \diff -> return (day1 `diff` day2)
  where day1 = fromGregorian (fst (toOrdinalDate now)) 1 1
        day2 = fromGregorian (succ (fst (toOrdinalDate now))) 1 1

mkTrans :: Day -> Integer -> Decimal -> Computer Transaction
mkTrans day days ratePerAnno = do
  bal <- gets balance
  srcAcc <- asks sourceAccount
  targetAcc <- asks targetAccount
  perDayScalar <- daysInYear day
  let t = nulltransaction
          { tdate          = day
          , tdescription   = T.pack $ showPercent ratePerAnno ++ " interest for " ++ showMixedAmount bal ++ " over " ++ show days ++ " days"
          , tpostings      = [pTarget,pSource]
          }
      pTarget = nullposting
          { paccount       = targetAcc
          , pamount        = mixed [ a { aquantity = (aquantity a * ratePerAnno) / fromInteger perDayScalar * fromInteger days } | a <- amounts bal ]
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
