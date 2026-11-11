module Hledger.Interest.Rate ( Rate, constant, scheduled, perAnno, perAnnoSchedule ) where

import Data.Time.Calendar
import Data.Decimal
import Data.List (sortOn, find)

type Rate = Day -> (Day,Decimal)

constant :: Decimal -> Rate
constant rate _ = (day 999999 12 31, rate)

-- | Compatibility aliases; posting boundaries are independent of the rate.
perAnno :: Decimal -> Rate
perAnno = constant

perAnnoSchedule :: [(Day,Decimal)] -> Rate
perAnnoSchedule = scheduled

-- | Rate-change boundaries only; calendar-year handling belongs to the computer.
scheduled :: [(Day,Decimal)] -> Rate
scheduled schedule date = (endOfPeriod, effectiveRate)
  where
    (endOfPeriod, effectiveRate) =
      case find (\(fromDate, toDate,  _) -> fromDate<=date && date<=toDate) fromToSchedule of
        -- date precedes the schedule: no interest until the first rate takes effect
        Nothing -> (beforeSchedule, 0)
        Just (_,toDate,rate) -> (toDate, rate)
    beforeSchedule =
      case sortedSchedule of
        (firstDate, _):_ -> pred firstDate
        []               -> day 999999 12 31
    fromToSchedule =
      -- Convert the list of (startDate,rate) to (startDate,endDate,rate) intervals, where
      -- the end date of the interval is the day before the start of the next interval,
      -- and the last interval goes to infinity
      zipWith
        (\(from, rate) (to, _) -> (from, pred to, rate))
        sortedSchedule
        (drop 1 sortedSchedule ++ [(day 999999 1 1,0)])
    sortedSchedule = sortOn fst schedule

day :: Integer -> Int -> Int -> Day
day = fromGregorian
