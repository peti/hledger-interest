module HistoricalRates (historicalModels) where

import Data.Decimal (Decimal)
import Data.Time.Calendar (Day, fromGregorian)

-- Historical snapshots from the removed built-in modes. Keep these data in
-- tests only: the executable receives ordinary annual schedules and explicit
-- day-count flags. Schedules have zero interest before the first entry and
-- keep the final rate indefinitely; they do not enforce the old coverage limits.
historicalModels :: [(String, [String])]
historicalModels =
  [ ("bgb288", ["--act", scheduleArgument [(date, base + 5/100) | (date, base) <- basiszinsTable]])
  , ("ing-diba", ["--act", scheduleArgument ingDibaSchedule])
  , ("db24", ["--30E-360", "--no-year-end-posting", scheduleArgument db24Schedule])
  ]

scheduleArgument :: [(Day, Decimal)] -> String
scheduleArgument schedule = "--schedule=" ++ show schedule

day :: Integer -> Int -> Int -> Day
day = fromGregorian

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

-- Preserve the original annual boundaries, including unchanged rates.
ingDibaSchedule :: [(Day, Decimal)]
ingDibaSchedule =
  [ (day 2009 01 01, 150 / 10000)
  , (day 2010 01 01, 150 / 10000)
  , (day 2011 01 01, 150 / 10000)
  , (day 2011 07 15, 175 / 10000)
  ]

-- Suppressing year-end postings reproduces the original DB24 compounding.
db24Schedule :: [(Day, Decimal)]
db24Schedule =
  [ (day 2000 10 06, 638 / 10000)
  , (day 2010 09 16, 415 / 10000)
  ]
