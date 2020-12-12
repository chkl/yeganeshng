-- boilerplate {{{
{-# LANGUAGE TupleSections #-}

module Version (CurrentFormat, parseCurrentFormat, pprintCurrentFormat, versionNumber, versionYeganesh, versionStrip) where

import Control.Monad (liftM2)
import Data.Char (isSpace)
import Data.Map (Map, assocs, elems, empty, fromList)
import Data.Time (UTCTime, getCurrentTime)
import Data.Version (showVersion)
import Lex (ReadP, char, lexDecNumber, lexString, readP_to_S, readS_to_P, sepBy, string, (+++))
import Paths_yeganesh (version)

type CurrentFormat = FormatV4

versionNumber :: String
versionYeganesh :: String
versionStrip :: String
pprintCurrentFormat :: CurrentFormat -> String
parseCurrentFormat :: String -> IO CurrentFormat
versionNumber = showVersion version

versionYeganesh = "yeganesh version " ++ versionNumber

versionStrip = "yeganesh-strip version " ++ versionNumber

pprintCurrentFormat = pprintFormatV4

parseCurrentFormat = parseFormatV4

-- }}}
-- v4 {{{
type FormatV4 = FormatV3

pprintFormatV4 :: FormatV4 -> String
pprintFormatV4 (now, cv) =
  unlines $
    show now :
      [show n ++ " " ++ line | (line, n) <- assocs cv]

parseFormatV4 :: String -> IO FormatV4
parseFormatV4 = parse v4 parseFormatV3 return

v4 :: String -> Maybe FormatV4
v4 = parseLines . lines
  where
    parseLines [] = Nothing
    parseLines (h : m) = liftM2 combine (parseHeader h) (mapM parseLine m)

    combine h m = (h, fromList [(b, a) | (a, b) <- m])
    parseHeader = readMaybe readp
    parseLine s = case break isSpace s of
      (n, ' ' : line) -> fmap (,line) (readMaybe lexDouble n)
      _ -> Nothing

-- }}}
-- v3 {{{
type FormatV3 = FormatV2

-- be stricter about what you accept, but faster at parsing
parseFormatV3 :: String -> IO FormatV3
parseFormatV3 = parse (readMaybe v3) parseFormatV2 upgradeFormatV2

v3 :: ReadP FormatV3
v3 = lexTuple readp (lexMap lexString lexDouble)
  where
    lexTuple lexA lexB = do
      _ <- char '('
      a <- lexA
      _ <- char ','
      b <- lexB
      _ <- char ')'
      return (a, b)
    lexList lexElem = do
      _ <- char '['
      vs <- sepBy lexElem (char ',') +++ return []
      _ <- char ']'
      return vs
    lexMap lexKey lexValue = do
      _ <- string "fromList "
      fmap fromList (lexList (lexTuple lexKey lexValue))

-- }}}
-- v2 {{{
type FormatV2 = (UTCTime, FormatV1)

upgradeFormatV2 :: FormatV2 -> IO FormatV3
upgradeFormatV2 = return

parseFormatV2 :: String -> IO FormatV2
parseFormatV2 = parse (readMaybe readp) parseFormatV1 upgradeFormatV1

-- }}}
-- v1 {{{
type FormatV1 = Map String Double

upgradeFormatV1 :: FormatV1 -> IO FormatV2
upgradeFormatV1 v1 = fmap (flip (,) popularities) getCurrentTime
  where
    maximal = case (elems v1, maximum (elems v1)) of
      ([], _) -> 1
      (_, 0) -> 1
      (_, m) -> m
    popularities = fmap (/ maximal) v1

parseFormatV1 :: String -> IO FormatV1
parseFormatV1 = parse (readMaybe readp) (const (return empty)) return

-- }}}
-- utilities {{{
lexDouble :: ReadP Double
lexDouble = fmap fromRational lexDecNumber

parse :: (String -> Maybe new) -> (String -> IO old) -> (old -> IO new) -> (String -> IO new)
parse new old upgrade s = maybe (old s >>= upgrade) return (new s)

readMaybe :: ReadP a -> String -> Maybe a
readMaybe p s = case readP_to_S p s of
  [(x, unparsed)] | all isSpace unparsed -> Just x
  _ -> Nothing

readp :: Read a => ReadP a
readp = readS_to_P reads

-- }}}
