{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Lib where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as S
import           Data.Functor.Identity
import           Data.Word

data Token
  = OpenObject
  | CloseObject
  | String ByteString
  | Colon
  | Comma
  | OpenArray
  | CloseArray
  | Number Double
  | Boolean Bool
  | Null
  deriving (Show, Ord, Eq)

tokenize
  :: Applicative f
  => (Token -> f ())
  -> ByteString
  -> f ()
tokenize handleToken str = go 0
 where
  go (skipSpaces str -> index) = case S.uncons (S.drop index str) of
    Just (x, _)
      | x == doubleQuote -> findClosingQuote index (index + 1)
      | x == openBrace -> handleToken OpenObject *> go (index + 1)
      | x == closeBrace -> handleToken CloseObject *> go (index + 1)
      | x == openBracket -> handleToken OpenArray *> go (index + 1)
      | x == closeBracket -> handleToken CloseArray *> go (index + 1)
      | x == comma -> handleToken Comma *> go (index + 1)
      | x == colon -> handleToken Colon *> go (index + 1)
      | x == minusChar -> number negate (index + 1)
      | isNumberChar x -> number id index
      | S.isPrefixOf "true" (S.drop index str) -> handleToken (Boolean True) *> go (index + 4)
      | S.isPrefixOf "false" (S.drop index str) -> handleToken (Boolean False) *> go (index + 4)
      | S.isPrefixOf "null" (S.drop index str) -> handleToken Null *> go (index + 4)
      | otherwise -> error ("broke " ++ show (substring str (index - 5) (index + 10)))
    _ -> pure ()
  -- 'start' is the index of the opening quote.
  -- 'index' is the index to start searching at.
  -- 'index' advances with each escaped quote found.
  findClosingQuote start index = case elemIndexFrom doubleQuote str index of
    Nothing     -> error "close"
    Just index' -> if isEscaped index'
      then findClosingQuote start (index' + 1)
      else handleToken (String (substring str (start + 1) index'))
        *> go (index' + 1)
  -- TODO: Maybe make this tail call itself?
  isEscaped index =
    let x = S.index str (index - 1)
    in  x == escapeSlash && not (isEscaped (index - 1))
  number sign index =
    let (int, index') = parseNumber str index
    in decimal sign int index'
  decimal sign int index = case S.uncons (S.drop index str) of
    Just (x, _)
      | x == period ->
        let (dec, index') = parseDecimal str (index + 1)
        in expon (sign (int + dec)) index'
    _ -> expon (sign int) index
  expon n index = case S.uncons (S.drop index str) of
    Just (x, _)
      | isExponent x ->
        let (e, index') = parseNumber str (index + 1) :: (Integer, Int)
        in handleToken (Number (n * fromInteger (10 ^ e))) *> go index'
    _ -> handleToken (Number n) *> go index
{-# INLINE tokenize #-}
{-# SPECIALIZE tokenize :: (Token -> Identity ()) -> ByteString -> Identity () #-}
{-# SPECIALIZE tokenize :: (Token -> IO ()) -> ByteString -> IO () #-}

tokenize' :: ByteString -> ()
tokenize' str =
  runIdentity $ tokenize (const (pure ())) str
{-# NOINLINE tokenize' #-}

tokenize'' :: ByteString -> IO ()
tokenize'' = tokenize print
{-# NOINLINE tokenize'' #-}

--------------------------------------------------------------------------------
-- ByteString utilities

-- | A fast space skipping function.
skipSpaces :: ByteString -> Int -> Int
skipSpaces str i = case S.uncons (S.drop i str) of
  Just (x, _) | isSpaceChar x -> skipSpaces str (i + 1)
  _           -> i
{-# INLINE skipSpaces #-}

-- | Get a substring of a string.
substring :: ByteString -> Int -> Int -> ByteString
substring s start end = S.take (end - start) (S.drop start s)
{-# INLINE substring #-}

-- | Get index of an element starting from offset.
elemIndexFrom :: Word8 -> ByteString -> Int -> Maybe Int
elemIndexFrom c str offset = fmap (+offset) (S.elemIndex c (S.drop offset str))
-- Without the INLINE below, the whole function is twice as slow and
-- has linear allocation.
{-# INLINE elemIndexFrom #-}

parseNumber :: Num a => ByteString -> Int -> (a, Int)
parseNumber str = go 0
 where
  go n index = case S.uncons (S.drop index str) of
    Just (x, _) | isNumberChar x -> go (n * 10 + digitToNum x) (index + 1)
    _                            -> (n, index)
{-# INLINE parseNumber #-}

parseDecimal :: Fractional a => ByteString -> Int -> (a, Int)
parseDecimal str = go 0 10
 where
  go n den index = case S.uncons (S.drop index str) of
    Just (x, _) | isNumberChar x ->
      go (n + (digitToNum x / den)) (den * 10) (index + 1)
    _ -> (n, index)
{-# INLINE parseDecimal #-}

--------------------------------------------------------------------------------
-- Character types

-- | The only valid whitespace in a JSON document is space, newline,
-- carriage return, and tab.
isSpaceChar :: Word8 -> Bool
isSpaceChar c = c == 0x20 || c == 0x0a || c == 0x0d || c == 0x09
{-# INLINE isSpaceChar #-}

isNumberChar :: Word8 -> Bool
isNumberChar c = c >= 48 && c <= 57
{-# INLINE isNumberChar #-}

digitToNum :: Num a => Word8 -> a
digitToNum x = fromIntegral (x - 48)
{-# INLINE digitToNum #-}

isExponent :: Word8 -> Bool
isExponent x = x == 69 || x == 101
{-# INLINE isExponent #-}

minusChar :: Word8
minusChar = 45

period :: Word8
period = 46

doubleQuote :: Word8
doubleQuote = 34

comma :: Word8
comma = 44

colon :: Word8
colon = 58

escapeSlash :: Word8
escapeSlash = 92

-- [
openBracket :: Word8
openBracket = 91

closeBracket :: Word8
closeBracket = 93

-- {
openBrace :: Word8
openBrace = 123

closeBrace :: Word8
closeBrace = 125
