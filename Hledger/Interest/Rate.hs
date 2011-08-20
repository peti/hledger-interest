module Hledger.Interest.Rate ( Rate, perAnno, perMonth, bgb288 ) where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

type Rate = Day -> (Day,Double)

perAnno :: Double -> Rate
perAnno rate date = (lastDayOfYear date, rate)

perMonth :: Double -> Rate
perMonth rate date = (date', rate)
  where
    date' = 16 `addDays` (lastDayOfMonth date)

day :: Integer -> Int -> Int -> Day
day = fromGregorian

bgb288 :: Rate
bgb288 = basiszins (5/100)

basiszins :: Double -> Rate
basiszins r date = (to, (r + p))
  where
    (from,to,p) = head (dropWhile (\(_,to',_) -> to' < date) basiszinsTable)

basiszinsTable :: [(Day, Day, Double)]
basiszinsTable =
  [ (day 2002 01 01, day 2002 06 30, 257 / 10000)
  , (day 2002 07 01, day 2002 12 31, 247 / 10000)
  , (day 2003 01 01, day 2003 06 30, 197 / 10000)
  , (day 2003 07 01, day 2003 12 31, 122 / 10000)
  , (day 2004 01 01, day 2004 06 30, 114 / 10000)
  , (day 2004 07 01, day 2004 12 31, 113 / 10000)
  , (day 2005 01 01, day 2005 06 30, 121 / 10000)
  , (day 2005 07 01, day 2005 12 31, 117 / 10000)
  , (day 2006 01 01, day 2006 06 30, 137 / 10000)
  , (day 2006 07 01, day 2006 12 31, 195 / 10000)
  , (day 2007 01 01, day 2007 06 30, 270 / 10000)
  , (day 2007 07 01, day 2007 12 31, 319 / 10000)
  , (day 2008 01 01, day 2008 06 30, 332 / 10000)
  , (day 2008 07 01, day 2008 12 31, 319 / 10000)
  , (day 2009 01 01, day 2009 06 30, 162 / 10000)
  , (day 2009 07 01, day 2009 12 31,  12 / 10000)
  , (day 2010 01 01, day 2010 06 30,  12 / 10000)
  , (day 2010 07 01, day 2010 12 31,  12 / 10000)
  , (day 2011 01 01, day 2011 06 30,  12 / 10000)
  , (day 2011 07 01, day 2999 12 31,  37 / 10000)
  ]

lastDayOfMonth :: Day -> Day
lastDayOfMonth now = day y m (gregorianMonthLength y m) where (y,m,d) = toGregorian now

firstDayOfMonth :: Day -> Day
firstDayOfMonth now = day y m 1 where (y,m,d) = toGregorian now

firstDayOfYear :: Day -> Day
firstDayOfYear now = day (fst (toOrdinalDate now)) 1 1

lastDayOfYear :: Day -> Day
lastDayOfYear now = day (fst (toOrdinalDate now)) 12 31
