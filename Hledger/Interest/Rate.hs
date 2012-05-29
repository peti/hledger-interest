module Hledger.Interest.Rate ( Rate, perAnno, parseInterestRateFile ) where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Hledger.Data.Dates ( failIfInvalidYear )
import Text.ParserCombinators.Parsec
import Data.Decimal

type Rate = Day -> (Day,Decimal)

perAnno :: Decimal -> Rate
perAnno rate date = (day (fst (toOrdinalDate date)) 12 31, rate)

day :: Integer -> Int -> Int -> Day
day = fromGregorian

parseInterestRateFile :: SourceName -> IO Rate
parseInterestRateFile = fmap table2rate . parseInterestRateTable

parseInterestRateTable :: SourceName -> IO [(Day,Decimal)]
parseInterestRateTable source = do
  buf <- readFile source
  either (fail . show) return (parse pInterestTable source buf)

table2rate :: [(Day,Decimal)] -> Rate
table2rate [] _     = error "HLedger.Interest.Rate.table2rate cannot be used with an empty table"
table2rate irt date =
  case irt of
    (d1,r1):(d2,r2):ds  -> if d1 <= date && d2 > date then ((-1) `addDays` d2,r1) else table2rate ((d2,r2):ds) date
    (_,r):[]            -> perAnno r date
    []                  -> error "HLedger.Interest.Rate.table2rate: the impossible case has happened"

pInterestTable :: GenParser Char st [(Day,Decimal)]
pInterestTable = sepEndBy1 pInterestTableLine newline

pInterestTableLine :: GenParser Char st (Day,Decimal)
pInterestTableLine = do
  d <- pIsoDate
  _ <- skipMany (oneOf " \t")
  rate <- pDecimal
  _ <- skipMany (oneOf " \t")
  return (d,rate)

pIsoDate :: GenParser Char st Day
pIsoDate = do
  y <- many1 digit
  failIfInvalidYear y
  _ <- pDateSep
  m <- many1 digit
  -- failIfInvalidMonth m
  _ <- pDateSep
  d <- many1 digit
  -- failIfInvalidDay d
  return (fromGregorian (read y) (read m) (read d))

pDateSep :: GenParser Char st Char
pDateSep = oneOf "/-."

pDecimal :: GenParser Char st Decimal
pDecimal = do
  num <- many1 digit
  frac <- option "0" (char '.' >> many1 digit)
  return (read (num ++ "." ++ frac))
