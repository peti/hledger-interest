module Hledger.Interest.DayCountConvention
  ( DayCountConvention
  , diffAct
  , diff30_360
  , diff30E_360
  , diff30E_360isda
  )
  where

import Control.Exception ( assert )
import Data.Time.Calendar

type DayCountConvention = Day -> Day -> Integer

diffAct :: DayCountConvention
diffAct date1 date2 = assert (date1 <= date2) $ fromInteger (date2 `diffDays` date1)

mkDiff30_360 :: (Integer,Int,Int) -> (Integer,Int,Int) -> Integer
mkDiff30_360 (y1,m1,d1) (y2,m2,d2) = 360*(y2-y1) + 30 * toInteger (m2-m1) + toInteger (d2-d1)

-- The un-corrected naked formular.

diff30_360 :: DayCountConvention
diff30_360 date1 date2 = assert (date1 <= date2) $ mkDiff30_360 (toGregorian date1) (toGregorian date2)

-- No month has more than 30 days, but February may have 28 or 29; i.e.
-- there are 32 days between 2003-02-28 and 2003-03-31. Commonly known
-- as "Deutsche Zinsmethode 30 / 360".

diff30E_360 :: DayCountConvention
diff30E_360 date1 date2 = assert (date1 <= date2) $ mkDiff30_360 (y1, m1, min 30 d1) (y2, m2, min 30 d2)
  where
    (y1,m1,d1) = toGregorian date1
    (y2,m2,d2) = toGregorian date2

-- This variant additionally normalizes end-of-months to 30, i.e. there
-- are 30 days between 2003-02-28 and 2003-03-31.

diff30E_360isda :: DayCountConvention
diff30E_360isda date1 date2 = assert (date1 <= date2) $ mkDiff30_360 (y1, m1, d1') (y2, m2, d2')
  where
    (y1,m1,d1) = toGregorian date1
    (y2,m2,d2) = toGregorian date2
    d1' = if d1 > 30 || d1 == gregorianMonthLength y1 m1 then 30 else d1
    d2' = if d1 > 30 || d2 == gregorianMonthLength y2 m2 then 30 else d2
