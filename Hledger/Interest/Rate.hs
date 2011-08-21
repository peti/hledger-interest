module Hledger.Interest.Rate ( Rate, perAnno, constant, bgb288, ingDiba ) where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

type Rate = Day -> (Day,Double)

constant :: Double -> Rate
constant rate _ = (day 999999 12 31, rate)

perAnno :: Double -> Rate
perAnno rate date = (day (fst (toOrdinalDate date)) 12 31, rate)

day :: Integer -> Int -> Int -> Day
day = fromGregorian

bgb288 :: Rate
bgb288 = basiszins (5/100)

basiszins :: Double -> Rate
basiszins r date = (to, r + p)
  where
    (_,to,p) = head (dropWhile (\(_,to',_) -> to' < date) basiszinsTable)

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

ingDiba :: Rate
ingDiba date = (to, p)
  where
    (_,to,p) = head (dropWhile (\(_,to',_) -> to' < date) ingDibaTable)

ingDibaTable :: [(Day, Day, Double)]
ingDibaTable =
  [ (day 2009 01 01, day 2009 12 31, 150 / 10000)
  , (day 2010 01 01, day 2010 12 31, 150 / 10000)
  , (day 2011 01 01, day 2011 07 14, 150 / 10000)
  , (day 2011 07 15, day 2999 12 31, 175 / 10000)
  ]
