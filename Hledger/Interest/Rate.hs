module Hledger.Interest.Rate ( Rate, perAnno, perAnnoSchedule, constant, bgb288, ingDiba, db24 ) where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Decimal
import Data.List (sortOn)

type Rate = Day -> (Day,Decimal)

constant :: Decimal -> Rate
constant rate _ = (day 999999 12 31, rate)

perAnno :: Decimal -> Rate
perAnno rate date = (yearEnd date, rate)

perAnnoSchedule :: [(Day,Decimal)] -> Rate
perAnnoSchedule schedule date = (yearEnd date, effectiveRate)
  where
    (_, effectiveRate) = last $ takeWhile (\(fromDate, _) -> fromDate<date) sortedSchedule
    sortedSchedule = sortOn fst schedule
        
day :: Integer -> Int -> Int -> Day
day = fromGregorian

yearEnd :: Day -> Day
yearEnd date = day (fst (toOrdinalDate date)) 12 31

bgb288 :: Rate
bgb288 = basiszins (5/100)

basiszins :: Decimal -> Rate
basiszins r date = (to, r + p)
  where
    (_,to,p) = head (dropWhile (\(_,to',_) -> to' < date) basiszinsTable)

basiszinsTable :: [(Day, Day, Decimal)]
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
  , (day 2011 01 01, day 2011 06 30,  12 / 10000)
  , (day 2011 07 01, day 2011 12 31,  37 / 10000)
  , (day 2012 01 01, day 2012 06 30,  12 / 10000)
  , (day 2012 07 01, day 2012 12 31,  12 / 10000)
  , (day 2013 01 01, day 2013 06 30, (-13) / 10000)
  , (day 2013 07 01, day 2013 12 31, (-38) / 10000)
  , (day 2014 01 01, day 2014 06 30, (-63) / 10000)
  , (day 2014 07 01, day 2014 12 31, (-73) / 10000)
  , (day 2015 01 01, day 2015 06 30, (-83) / 10000)
  , (day 2015 07 01, day 2015 12 31, (-83) / 10000)
  , (day 2016 01 01, day 2016 06 30, (-83) / 10000)
  , (day 2016 07 01, day 2016 12 31, (-88) / 10000)
  , (day 2017 01 01, day 2017 06 30, (-88) / 10000)
  , (day 2017 07 01, day 2017 12 31, (-88) / 10000)
  , (day 2018 01 01, day 2018 06 30, (-88) / 10000)
  , (day 2019 01 01, day 2019 06 30, (-88) / 10000)
  , (day 2019 07 01, day 2019 12 31, (-88) / 10000)
  , (day 2020 01 01, day 2020 06 30, (-88) / 10000)
  , (day 2020 07 01, day 2020 12 31, (-88) / 10000)
  , (day 2021 01 01, day 2021 06 30, (-88) / 10000)
  , (day 2021 07 01, day 2021 12 31, (-88) / 10000)
  , (day 2022 01 01, day 2022 06 30, (-88) / 10000)
  , (day 2022 07 01, day 2022 12 31, (-88) / 10000)
  , (day 2023 01 01, day 2023 06 30, 162 / 10000)
  , (day 2023 07 01, day 2023 12 31, 312 / 10000)
  , (day 2024 01 01, day 2024 06 30, 362 / 10000)
  , (day 2024 07 01, day 2024 12 31, 337 / 10000)
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
