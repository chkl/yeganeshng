-- stolen from Text.Read.Lex in GHC's base library

module Lex (lexDecNumber, lexString, module Text.ParserCombinators.ReadP) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Ratio
import Text.ParserCombinators.ReadP
import Prelude hiding (exp)

lexCharE :: ReadP (Char, Bool) -- "escaped or not"?
lexCharE =
  do
    c1 <- get
    if c1 == '\\'
      then do c2 <- lexEsc; return (c2, True)
      else do return (c1, False)
  where
    lexEsc =
      lexEscChar
        +++ lexNumeric
        +++ lexCntrlChar
        +++ lexAscii

    lexEscChar =
      do
        c <- get
        case c of
          'a' -> return '\a'
          'b' -> return '\b'
          'f' -> return '\f'
          'n' -> return '\n'
          'r' -> return '\r'
          't' -> return '\t'
          'v' -> return '\v'
          '\\' -> return '\\'
          '\"' -> return '\"'
          '\'' -> return '\''
          _ -> pfail

    lexNumeric =
      do
        base <- lexBaseChar <++ return 10
        n <- lexInteger base
        guard (n <= toInteger (ord maxBound))
        return (chr (fromInteger n))

    lexCntrlChar =
      do
        _ <- char '^'
        c <- get
        case c of
          '@' -> return '\^@'
          'A' -> return '\^A'
          'B' -> return '\^B'
          'C' -> return '\^C'
          'D' -> return '\^D'
          'E' -> return '\^E'
          'F' -> return '\^F'
          'G' -> return '\^G'
          'H' -> return '\^H'
          'I' -> return '\^I'
          'J' -> return '\^J'
          'K' -> return '\^K'
          'L' -> return '\^L'
          'M' -> return '\^M'
          'N' -> return '\^N'
          'O' -> return '\^O'
          'P' -> return '\^P'
          'Q' -> return '\^Q'
          'R' -> return '\^R'
          'S' -> return '\^S'
          'T' -> return '\^T'
          'U' -> return '\^U'
          'V' -> return '\^V'
          'W' -> return '\^W'
          'X' -> return '\^X'
          'Y' -> return '\^Y'
          'Z' -> return '\^Z'
          '[' -> return '\^['
          '\\' -> return '\^\'
          ']' -> return '\^]'
          '^' -> return '\^^'
          '_' -> return '\^_'
          _ -> pfail

    lexAscii =
      do
        choice
          [ (string "SOH" >> return '\SOH')
              <++ (string "SO" >> return '\SO'),
            -- \SO and \SOH need maximal-munch treatment
            -- See the Haskell report Sect 2.6

            string "NUL" >> return '\NUL',
            string "STX" >> return '\STX',
            string "ETX" >> return '\ETX',
            string "EOT" >> return '\EOT',
            string "ENQ" >> return '\ENQ',
            string "ACK" >> return '\ACK',
            string "BEL" >> return '\BEL',
            string "BS" >> return '\BS',
            string "HT" >> return '\HT',
            string "LF" >> return '\LF',
            string "VT" >> return '\VT',
            string "FF" >> return '\FF',
            string "CR" >> return '\CR',
            string "SI" >> return '\SI',
            string "DLE" >> return '\DLE',
            string "DC1" >> return '\DC1',
            string "DC2" >> return '\DC2',
            string "DC3" >> return '\DC3',
            string "DC4" >> return '\DC4',
            string "NAK" >> return '\NAK',
            string "SYN" >> return '\SYN',
            string "ETB" >> return '\ETB',
            string "CAN" >> return '\CAN',
            string "EM" >> return '\EM',
            string "SUB" >> return '\SUB',
            string "ESC" >> return '\ESC',
            string "FS" >> return '\FS',
            string "GS" >> return '\GS',
            string "RS" >> return '\RS',
            string "US" >> return '\US',
            string "SP" >> return '\SP',
            string "DEL" >> return '\DEL'
          ]

-- ---------------------------------------------------------------------------
-- string literal

lexString :: ReadP String
lexString =
  do
    _ <- char '"'
    body id
  where
    body f =
      do
        (c, esc) <- lexStrItem
        if c /= '"' || esc
          then body (f . (c :))
          else
            let s = f ""
             in return s

    lexStrItem =
      (lexEmpty >> lexStrItem)
        +++ lexCharE

    lexEmpty =
      do
        _ <- char '\\'
        c <- get
        case c of
          '&' -> do return ()
          _ | isSpace c -> do skipSpaces; _ <- char '\\'; return ()
          _ -> do pfail

lexBaseChar :: ReadP Int
-- Lex a single character indicating the base; fail if not there
lexBaseChar = do
  c <- get
  case c of
    'o' -> return 8
    'O' -> return 8
    'x' -> return 16
    'X' -> return 16
    _ -> pfail

type Base = Int

type Digits = [Int]

lexDecNumber :: ReadP Rational
lexDecNumber =
  do
    xs <- lexDigits 10
    mFrac <- lexFrac <++ return Nothing
    mExp <- lexExp <++ return Nothing
    return (value xs mFrac mExp)
  where
    value xs mFrac mExp = valueFracExp (val 10 0 xs) mFrac mExp

    valueFracExp ::
      Integer ->
      Maybe Digits ->
      Maybe Integer ->
      Rational
    valueFracExp a Nothing Nothing =
      a % 1 -- 43
    valueFracExp a Nothing (Just exp)
      | exp >= 0 = a * (10 ^ exp) % 1 -- 43e7
      | otherwise = a % (10 ^ (- exp)) -- 43e-7
    valueFracExp a (Just fs) mExp -- 4.3[e2]
      =
      (fracExp (fromMaybe 0 mExp) a fs)

-- Be a bit more efficient in calculating the Rational.
-- Instead of calculating the fractional part alone, then
-- adding the integral part and finally multiplying with
-- 10 ^ exp if an exponent was given, do it all at once.

lexFrac :: ReadP (Maybe Digits)
-- Read the fractional part; fail if it doesn't
-- start ".d" where d is a digit
lexFrac = do
  _ <- char '.'
  fraction <- lexDigits 10
  return (Just fraction)

lexExp :: ReadP (Maybe Integer)
lexExp = do
  _ <- char 'e' +++ char 'E'
  exp <- signedExp +++ lexInteger 10
  return (Just exp)
  where
    signedExp =
      do
        c <- char '-' +++ char '+'
        n <- lexInteger 10
        return (if c == '-' then - n else n)

lexDigits :: Int -> ReadP Digits
-- Lex a non-empty sequence of digits in specified base
lexDigits base =
  do
    s <- look
    xs <- scan s id
    guard (not (null xs))
    return xs
  where
    scan (c : cs) f = case valDig base c of
      Just n -> do _ <- get; scan cs (f . (n :))
      Nothing -> do return (f [])
    scan [] f = do return (f [])

lexInteger :: Base -> ReadP Integer
lexInteger base =
  do
    xs <- lexDigits base
    return (val (fromIntegral base) 0 xs)

val :: Num a => a -> a -> Digits -> a
-- val base y [d1,..,dn] = y ++ [d1,..,dn], as it were
val _ y [] = y
val base y (x : xs) = y' `seq` val base y' xs
  where
    y' = y * base + fromIntegral x

-- Calculate a Rational from the exponent [of 10 to multiply with],
-- the integral part of the mantissa and the digits of the fractional
-- part. Leaving the calculation of the power of 10 until the end,
-- when we know the effective exponent, saves multiplications.
-- More importantly, this way we need at most one gcd instead of three.
--
-- frac was never used with anything but Integer and base 10, so
-- those are hardcoded now (trivial to change if necessary).
fracExp :: Integer -> Integer -> Digits -> Rational
fracExp exp mant []
  | exp < 0 = mant % (10 ^ (- exp))
  | otherwise = fromInteger (mant * 10 ^ exp)
fracExp exp mant (d : ds) = exp' `seq` mant' `seq` fracExp exp' mant' ds
  where
    exp' = exp - 1
    mant' = mant * 10 + fromIntegral d

valDig :: (Eq a, Num a) => a -> Char -> Maybe Int
valDig 8 c
  | '0' <= c && c <= '7' = Just (ord c - ord '0')
  | otherwise = Nothing
valDig 10 c = valDecDig c
valDig 16 c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | 'a' <= c && c <= 'f' = Just (ord c - ord 'a' + 10)
  | 'A' <= c && c <= 'F' = Just (ord c - ord 'A' + 10)
  | otherwise = Nothing
valDig _ _ = error "valDig: Bad base"

valDecDig :: Char -> Maybe Int
valDecDig c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | otherwise = Nothing
