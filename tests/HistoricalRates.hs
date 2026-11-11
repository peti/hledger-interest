module HistoricalRates (historicalModels) where

import Data.Decimal (Decimal)
import Data.Time.Calendar (Day, fromGregorian)

-- Historical snapshots from the removed built-in modes. Keep these data in
-- tests only: the executable receives ordinary annual schedules and explicit
-- day-count flags. Schedules have zero interest before the first entry and
-- keep the final rate indefinitely; they do not enforce the old coverage limits.
historicalModels :: [(String, [String])]
historicalModels =
  [ ("bgb288", ["--act", "--schedule-file=tests/fixtures/bgb288.rates"])
  , ("ing-diba", ["--act", scheduleArgument ingDibaSchedule])
  , ("db24", ["--30E-360", "--post-on-rate-changes", scheduleArgument db24Schedule])
  ]

scheduleArgument :: [(Day, Decimal)] -> String
scheduleArgument schedule = "--schedule=" ++ show schedule

day :: Integer -> Int -> Int -> Day
day = fromGregorian

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
