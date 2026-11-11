module Hledger.Interest.Rate ( Rate, perAnno, perAnnoSchedule, constant, bgb288, ingDiba, db24 ) where

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

-- | Interest according to German BGB §288: the base rate published by the
-- Deutsche Bundesbank (BGB §247) plus 5 percentage points.
--
-- >>> bgb288 (day 2011 08 15)
-- (2011-12-31,0.0537)
-- >>> bgb288 (day 2018 07 01)
-- (2018-12-31,0.0412)
-- >>> bgb288 (day 2023 06 01)
-- (2023-06-30,0.0662)
-- >>> bgb288 (day 2026 07 01)
-- (2026-12-31,0.0652)
bgb288 :: Rate
bgb288 = basiszins (5/100)

basiszins :: Decimal -> Rate
basiszins r date =
  case dropWhile (\(_,to',_) -> to' < date) basiszinsPeriods of
    (_,to,p) : _ -> (to, r + p)
    [] -> error ("hledger-interest: the BGB base rate after " ++ show lastEnd ++
                 " is not known yet; please update basiszinsTable in Hledger/Interest/Rate.hs")
  where
    lastEnd = case reverse basiszinsPeriods of
                (_,to,_) : _ -> to
                []           -> error "basiszinsTable is empty"

-- | Turn the start dates of 'basiszinsTable' into (from, to, rate) periods.
-- Each period ends the day before the next one starts; the last one is
-- valid for six months.
basiszinsPeriods :: [(Day, Day, Decimal)]
basiszinsPeriods = zipWith (\(from, p) to -> (from, to, p)) basiszinsTable ends
  where
    starts = map fst basiszinsTable
    ends   = map pred (drop 1 starts ++ map (addGregorianMonthsClip 6) (take 1 (reverse starts)))

-- | Basiszinssatz according to BGB §247, keyed by the date it takes effect.
-- Source: https://www.bundesbank.de/de/bundesbank/organisation/agb-und-regelungen/basiszinssatz-607820
basiszinsTable :: [(Day, Decimal)]
basiszinsTable =
  [ (day 2002 01 01,  257 / 10000)
  , (day 2002 07 01,  247 / 10000)
  , (day 2003 01 01,  197 / 10000)
  , (day 2003 07 01,  122 / 10000)
  , (day 2004 01 01,  114 / 10000)
  , (day 2004 07 01,  113 / 10000)
  , (day 2005 01 01,  121 / 10000)
  , (day 2005 07 01,  117 / 10000)
  , (day 2006 01 01,  137 / 10000)
  , (day 2006 07 01,  195 / 10000)
  , (day 2007 01 01,  270 / 10000)
  , (day 2007 07 01,  319 / 10000)
  , (day 2008 01 01,  332 / 10000)
  , (day 2008 07 01,  319 / 10000)
  , (day 2009 01 01,  162 / 10000)
  , (day 2009 07 01,   12 / 10000)
  , (day 2010 01 01,   12 / 10000)
  , (day 2010 07 01,   12 / 10000)
  , (day 2011 01 01,   12 / 10000)
  , (day 2011 07 01,   37 / 10000)
  , (day 2012 01 01,   12 / 10000)
  , (day 2012 07 01,   12 / 10000)
  , (day 2013 01 01, (-13) / 10000)
  , (day 2013 07 01, (-38) / 10000)
  , (day 2014 01 01, (-63) / 10000)
  , (day 2014 07 01, (-73) / 10000)
  , (day 2015 01 01, (-83) / 10000)
  , (day 2015 07 01, (-83) / 10000)
  , (day 2016 01 01, (-83) / 10000)
  , (day 2016 07 01, (-88) / 10000)
  , (day 2017 01 01, (-88) / 10000)
  , (day 2017 07 01, (-88) / 10000)
  , (day 2018 01 01, (-88) / 10000)
  , (day 2018 07 01, (-88) / 10000)
  , (day 2019 01 01, (-88) / 10000)
  , (day 2019 07 01, (-88) / 10000)
  , (day 2020 01 01, (-88) / 10000)
  , (day 2020 07 01, (-88) / 10000)
  , (day 2021 01 01, (-88) / 10000)
  , (day 2021 07 01, (-88) / 10000)
  , (day 2022 01 01, (-88) / 10000)
  , (day 2022 07 01, (-88) / 10000)
  , (day 2023 01 01,  162 / 10000)
  , (day 2023 07 01,  312 / 10000)
  , (day 2024 01 01,  362 / 10000)
  , (day 2024 07 01,  337 / 10000)
  , (day 2025 01 01,  227 / 10000)
  , (day 2025 07 01,  127 / 10000)
  , (day 2026 01 01,  127 / 10000)
  , (day 2026 07 01,  152 / 10000)
  ]

ingDiba :: Rate
ingDiba date = (to, p)
  where
    (_,to,p) = head (dropWhile (\(_,to',_) -> to' < date) ingDibaTable)

ingDibaTable :: [(Day, Day, Decimal)]
ingDibaTable =
  [ (day 2009 01 01, day 2009 12 31, 150 / 10000)
  , (day 2010 01 01, day 2010 12 31, 150 / 10000)
  , (day 2011 01 01, day 2011 07 14, 150 / 10000)
  , (day 2011 07 15, day 2999 12 31, 175 / 10000)
  ]

db24 :: Rate
db24 date = (to, p)
  where
    (_,to,p) = head (dropWhile (\(_,to',_) -> to' < date) db24Table)

db24Table :: [(Day, Day, Decimal)]
db24Table =
  [ (day 2000 10 06, day 2010 09 15, 638 / 10000)
  , (day 2010 09 16, day 2999 12 31, 415 / 10000)
  ]
