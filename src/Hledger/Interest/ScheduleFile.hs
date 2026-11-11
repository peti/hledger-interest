{-# LANGUAGE OverloadedStrings #-}

module Hledger.Interest.ScheduleFile (readScheduleFile) where

import Control.Monad (void, when)
import Control.Monad.State.Strict (evalStateT, lift)
import Data.Char (isSpace)
import Data.Decimal (Decimal)
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Calendar (Day, toGregorian)
import Hledger.Data (JournalParser, getCurrentDay, nulljournal)
import Hledger.Read.Common
  ( datep, emptyorcommentlinep, multilinecommentp
  , numberp, setYear
  )
import Hledger.Utils (TextParser)
import System.IO.Error (ioeGetErrorString, tryIOError)
import Text.Megaparsec
  ( (<|>), anySingle, eof, errorBundlePretty, getInput, lookAhead, manyTill, optional
  , runParser, satisfy, skipMany, skipSome, takeWhileP, (<?>)
  )
import Text.Megaparsec.Char (char, newline)

readScheduleFile :: FilePath -> IO (Either String [(Day, Decimal)])
readScheduleFile path = do
  contents <- tryIOError (TIO.readFile path)
  today <- getCurrentDay
  let (year, _, _) = toGregorian today
      parser = evalStateT (setYear year >> schedulep) nulljournal
      -- Like journal input, accept CRLF and a missing final newline. A final
      -- newline also lets hledger's comment parsers handle comments at EOF.
      normalise text = T.replace "\r\n" "\n" text <> "\n"
  pure $ case contents of
    Left err -> Left (path ++ ": " ++ ioeGetErrorString err)
    Right text -> case runParser parser path (normalise text) of
      Left err -> Left (errorBundlePretty err)
      Right schedule -> Right schedule

schedulep :: JournalParser Identity [(Day, Decimal)]
schedulep = catMaybes <$> manyTill itemp eof
  where
    itemp :: JournalParser Identity (Maybe (Day, Decimal))
    itemp = do
      input <- lift getInput
      -- Classify ignored lines without speculative consumption of indentation,
      -- so malformed entries are diagnosed at their beginning, in column 1.
      case T.uncons (T.dropWhile isHorizontalSpace input) of
        Just (c, _) | c `elem` ['\n', '#', ';', '*'] ->
          Nothing <$ lift emptyorcommentlinep
        _ -> Nothing <$ lift multilinecommentp <|> Just <$> entryp

entryp :: JournalParser Identity (Day, Decimal)
entryp = do
  first <- lift (lookAhead anySingle)
  when (isSpace first) $ fail "schedule entries must begin in column 1"
  date <- datep
  rate <- lift $ do
    skipSome horizontalSpace <?> "whitespace between date and rate"
    (quantity, _, _, _) <- numberp Nothing
    skipMany horizontalSpace
    percent <- optional (char '%')
    skipMany horizontalSpace
    void $ optional (char ';' *> takeWhileP Nothing (/= '\n'))
    void newline
    pure $ if isJust percent then quantity / 100 else quantity
  pure (date, rate)
  where
    horizontalSpace :: TextParser Identity Char
    horizontalSpace = satisfy isHorizontalSpace

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = isSpace c && c /= '\n'
