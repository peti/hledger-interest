module Main ( main ) where

import Hledger.Interest.DayCountConvention
import Hledger.Interest.Spec

import Hledger.Data
import Hledger.Read
import System.Environment
import Control.Monad.RWS
import Data.Time.Calendar.OrdinalDate
import Numeric

data Config = Config
  { interestAccount :: AccountName
  , sourceAccount :: AccountName
  , targetAccount :: AccountName
  , dayCountConvention :: DayCountConvention
  , interestSpec :: Specification
  }

data InterestState = IState
  { balancedUntil :: Day
  , balance :: MixedAmount
  }

type Computer = RWS Config [Transaction] InterestState

processTransaction :: Transaction -> Computer ()
processTransaction ts = do
  let day = maybe (tdate ts) id (teffectivedate ts)
  computeInterest day
  interestAcc <- asks interestAccount
  let posts = [ p | p <- tpostings ts, interestAcc == paccount p ]
  flip mapM_ posts $ \p -> do
    bal <- gets (amounts . balance)
    let bal' = bal ++ amounts (pamount p)
    modify (\st -> st { balance = normaliseMixedAmount (Mixed bal') })

computeInterest :: Day -> Computer ()
computeInterest day = do
  from <- gets balancedUntil
  bal <- gets balance
  ispec <- asks interestSpec
  let (_,endOfPeriod,ratePerAnno) = ispec from
      to = min day endOfPeriod
      newFrom = succ to
  when (to >= from && not (isZeroMixedAmount bal)) $ do
    diff <- asks dayCountConvention
    mkTrans from ((from `diff` to) + 1) ratePerAnno
  modify (\st -> st { balancedUntil = newFrom })
  when (newFrom < day) (computeInterest day)

daysInYear :: Day -> Computer Integer
daysInYear now = asks dayCountConvention >>= \diff -> return (day1 `diff` day2)
  where day1 = fromGregorian (fst (toOrdinalDate now)) 1 1
        day2 = fromGregorian (succ (fst (toOrdinalDate now))) 1 1

mkTrans :: Day -> Integer -> Double -> Computer ()
mkTrans day days ratePerAnno = do
  bal <- gets balance
  srcAcc <- asks sourceAccount
  targetAcc <- asks targetAccount
  perDayScalar <- daysInYear day
  let t = Transaction
          { tdate          = day
          , teffectivedate = Nothing
          , tstatus        = False
          , tcode          = ""
          , tdescription   = showPercent ratePerAnno ++ "% for " ++ show bal ++ " over " ++ show days ++ " days"
          , tcomment       = ""
          , tmetadata      = []
          , tpostings      = [pTarget,pSource]
          , tpreceding_comment_lines = ""
          }
      pTarget = Posting
          { pstatus        = False
          , paccount       = targetAcc
          , pamount        = Mixed [ a { quantity = (quantity a * ratePerAnno) / fromInteger perDayScalar * fromInteger days } | a <- amounts bal ]
          , pcomment       = ""
          , ptype          = RegularPosting
          , pmetadata      = []
          , ptransaction   = Just t
          }
      pSource = Posting
          { pstatus        = False
          , paccount       = srcAcc
          , pamount        = negate (pamount pTarget)
          , pcomment       = ""
          , ptype          = RegularPosting
          , pmetadata      = []
          , ptransaction   = Just t
          }
  tell [t]

showPercent :: Double -> String
showPercent r = showWith2Digits (r * 100)

showWith2Digits :: Double -> String
showWith2Digits r = showFFloat (Just 2) r ""

main :: IO ()
main = do
  jFile:interestAcc:srcAcc:targetAcc:[] <- getArgs
  Right jnl' <- readFile jFile >>= readJournal Nothing
  let jnl = filterJournalTransactionsByAccount [interestAcc] jnl'
      transactions = sortBy (\a b -> compare (tdate a) (tdate b)) (jtxns jnl)
      cfg = Config
            { interestAccount = interestAcc
            , sourceAccount = srcAcc
            , targetAccount = targetAcc
            , dayCountConvention = diffAct
            , interestSpec = bgb288 -- perAnno 0.05
            }
      st = IState
           { balancedUntil = nulldate
           , balance = nullmixedamt
           }
  thisDay <- getCurrentDay
  let ((),_,ts) = runRWS (mapM_ processTransaction transactions >> computeInterest thisDay) cfg st
  mapM_ (putStr . show) ts

{-
 runTest :: IO ()
 runTest = withArgs ["test.ledger", "Dept:Bank", "Expense:Interest", "Dept:Bank:Interest"] main
 -}
