module Hledger.Interest.Rate ( Rate, perAnno, perAnnoSchedule, constant ) where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Decimal
import Data.List (sortOn, find)

type Rate = Day -> (Day,Decimal)

constant :: Decimal -> Rate
constant rate _ = (day 999999 12 31, rate)

perAnno :: Decimal -> Rate
perAnno rate date = (yearEnd date, rate)

perAnnoSchedule :: [(Day,Decimal)] -> Rate
perAnnoSchedule schedule date = (effectiveUntil, effectiveRate)
  where
    effectiveUntil =
      -- make sure that end date we report does not cross the year boundary
      min (yearEnd date) endOfPeriod
    (endOfPeriod, effectiveRate) =
      case find (\(fromDate, toDate,  _) -> fromDate<=date && date<=toDate) fromToSchedule of
        -- date precedes the schedule: no interest until the first rate takes effect
        Nothing -> (beforeSchedule, 0)
        Just (_,toDate,rate) -> (toDate, rate)
    beforeSchedule =
      case sortedSchedule of
        (firstDate, _):_ -> pred firstDate
        []               -> yearEnd date
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

yearEnd :: Day -> Day
yearEnd date = day (fst (toOrdinalDate date)) 12 31
