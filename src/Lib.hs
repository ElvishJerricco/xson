{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Lib where

import           Control.Monad.State.Strict
import           Data.Aeson                 (Value)
import qualified Data.Aeson                 as Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as S
import           Data.Functor.Identity
import           Data.HashMap.Lazy          (HashMap)
import qualified Data.HashMap.Lazy          as HashMap
import           Data.Scientific
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as Text
import qualified Data.Vector                as Vector
import           Data.Word

data Token
  = OpenObject
  | CloseObject
  | String ByteString
  | Colon
  | Comma
  | OpenArray
  | CloseArray
  | Number Scientific
  | Boolean Bool
  | Null
  deriving (Show, Ord, Eq)

tokenize :: forall f. Applicative f => (Token -> f ()) -> ByteString -> f ()
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
        -- TODO: Replace these prefix checks with manually unrolled loops.
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
      else handleToken (String (escaped $ substring str (start + 1) index'))
        *> go (index' + 1)
  -- TODO: Maybe make this tail call itself?
  isEscaped index =
    let x = S.index str (index - 1)
    in  x == escapeSlash && not (isEscaped (index - 1))
  number :: (Integer -> Integer) -> Int -> f ()
  number sign index =
    let (int, index') = parseNumber str index
    in decimal sign int index'
  decimal :: (Integer -> Integer) -> Integer -> Int -> f ()
  decimal sign int index = case S.uncons (S.drop index str) of
    Just (x, _)
      | x == period ->
        let (dec, index') = parseNumber str (index + 1)
            e = index' - (index + 1)
        in expon (sign (int * 10^e + dec)) (-e) index'
    _ -> expon (sign int) 0 index
  expon :: Integer -> Int -> Int -> f ()
  expon n e index = case S.uncons (S.drop index str) of
    Just (x, _)
      | isExponent x ->
        let (e', index') = parseNumber str (index + 1) :: (Int, Int)
        in handleToken (Number (scientific n (e + e'))) *> go index'
    _ -> handleToken (Number (scientific n e)) *> go index
{-# INLINE tokenize #-}
{-# SPECIALIZE tokenize :: (Token -> Identity ()) -> ByteString -> Identity () #-}
{-# SPECIALIZE tokenize :: (Token -> IO ()) -> ByteString -> IO () #-}
{-# SPECIALIZE tokenize :: (Token -> State s ()) -> ByteString -> State s () #-}

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

escaped :: ByteString -> ByteString
escaped str = case S.elemIndex escapeSlash str of
  Nothing -> str
  Just index ->
    let preStr = substring str 0 index
    in  case S.uncons (S.drop (index + 1) str) of
          Just (x, str')
            | x == doubleQuote -> preStr `S.append` (doubleQuote `S.cons` escaped str')
            | x == escapeSlash -> preStr `S.append` (escapeSlash `S.cons` escaped str')
            | x == forwardSlash -> preStr `S.append` (forwardSlash `S.cons` escaped str')
            | x == bChar -> preStr `S.append` (backspace `S.cons` escaped str')
            | x == fChar -> preStr `S.append` (formFeed `S.cons` escaped str')
            | x == nChar -> preStr `S.append` (newLine `S.cons` escaped str')
            | x == rChar -> preStr `S.append` (carriageReturn `S.cons` escaped str')
            | x == tChar -> preStr `S.append` (tabChar `S.cons` escaped str')
            | x == uChar -> error "TODO: Implement unicode escapes"
            | otherwise -> preStr `S.append` (x `S.cons` escaped str')
          _ -> error "Invalid escape slash"
{-# INLINE escaped #-}

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
minusChar = S.index "-" 0

period :: Word8
period = S.index "." 0

doubleQuote :: Word8
doubleQuote = S.index "\"" 0

comma :: Word8
comma = S.index "," 0

colon :: Word8
colon = S.index ":" 0

escapeSlash :: Word8
escapeSlash = S.index "\\" 0

openBracket :: Word8
openBracket = S.index "[" 0

closeBracket :: Word8
closeBracket = S.index "]" 0

openBrace :: Word8
openBrace = S.index "{" 0

closeBrace :: Word8
closeBrace = S.index "}" 0

forwardSlash :: Word8
forwardSlash = S.index "/" 0

bChar :: Word8
bChar = S.index "b" 0

backspace :: Word8
backspace = S.index "\b" 0

fChar :: Word8
fChar = S.index "f" 0

formFeed :: Word8
formFeed = S.index "\f" 0

nChar :: Word8
nChar = S.index "n" 0

newLine :: Word8
newLine = S.index "\n" 0

rChar :: Word8
rChar = S.index "r" 0

carriageReturn :: Word8
carriageReturn = S.index "\r" 0

tChar :: Word8
tChar = S.index "t" 0

tabChar :: Word8
tabChar = S.index "\t" 0

uChar :: Word8
uChar = S.index "u" 0

--------------------------------------------------------------------------------

type ObjectDone = HashMap Text Value -> PState

data ObjectState
  = ObjectKey
  | ObjectColon !Text
  | ObjectValue !Text
  | ObjectComma

type ArrayDone = [Value] -> PState

data ArrayState
  = ArrayValue
  | ArrayComma

data PState
  = Start
  | Done !Value
  | PArray !ArrayState !ArrayDone
  | PObject !ObjectState !ObjectDone

parseTokens :: Token -> State PState ()
parseTokens = modify' . nextPState

nextPState :: Token -> PState -> PState
nextPState tok Start                            = startPState tok
nextPState _   (Done _                        ) = error "Done"
nextPState tok (PArray  ArrayValue        done) = nextArrayValue done tok
nextPState tok (PArray  ArrayComma        done) = nextArrayComma done tok
nextPState tok (PObject ObjectKey         done) = nextObjectKey done tok
nextPState tok (PObject (ObjectColon key) done) = nextObjectColon done key tok
nextPState tok (PObject (ObjectValue key) done) = nextObjectValue done key tok
nextPState tok (PObject ObjectComma       done) = nextObjectComma done tok

startPState :: Token -> PState
startPState OpenArray  = PArray ArrayValue (Done . doneArray)
startPState OpenObject = PObject ObjectKey (Done . Aeson.Object)
startPState (primVal -> Just v) = Done v
startPState _ = error "Expected value"

nextArrayValue :: ArrayDone -> Token -> PState
nextArrayValue done OpenArray =
  PArray ArrayValue $ \v -> PArray ArrayComma (done . (doneArray v:))
nextArrayValue done OpenObject =
  PObject ObjectKey $ \v -> PArray ArrayComma (done . (Aeson.Object v:))
nextArrayValue done CloseArray = done []
nextArrayValue done (primVal -> Just v) = PArray ArrayComma (done . (v:))
nextArrayValue _ _ = error "Expected value"

nextArrayComma :: ArrayDone -> Token -> PState
nextArrayComma done Comma      = PArray ArrayValue done
nextArrayComma done CloseArray = done []
nextArrayComma _    _          = error "Expected comma"

nextObjectKey :: ObjectDone -> Token -> PState
nextObjectKey done (String str) =
  PObject (ObjectColon (Text.decodeUtf8 str)) done
nextObjectKey done CloseObject = done HashMap.empty
nextObjectKey _    _           = error "Expected object key or close"

nextObjectColon :: ObjectDone -> Text -> Token -> PState
nextObjectColon done key Colon = PObject (ObjectValue key) done
nextObjectColon _    _   _     = error "Expected colon"

nextObjectValue :: ObjectDone -> Text -> Token -> PState
nextObjectValue done key OpenArray = PArray ArrayValue
  $ \v -> PObject ObjectComma (done . HashMap.insert key (doneArray v))
nextObjectValue done key OpenObject = PObject ObjectKey
  $ \v -> PObject ObjectComma (done . HashMap.insert key (Aeson.Object v))
nextObjectValue done key (primVal -> Just v) = PObject ObjectComma (done . HashMap.insert key v)
nextObjectValue _ _ _ = error "Expected value"

nextObjectComma :: ObjectDone -> Token -> PState
nextObjectComma done Comma       = PObject ObjectKey done
nextObjectComma done CloseObject = done HashMap.empty
nextObjectComma _    _           = error "Expected comma or close object"

primVal :: Token -> Maybe Value
primVal (String  str) = Just (Aeson.String (Text.decodeUtf8 str))
primVal (Number  n  ) = Just (Aeson.Number n)
primVal (Boolean b  ) = Just (Aeson.Bool b)
primVal Null          = Just Aeson.Null
primVal _             = Nothing

doneArray :: [Value] -> Value
doneArray = Aeson.Array . Vector.fromList

--------------------------------------------------------------------------------

parse :: ByteString -> Value
parse str = let Done v = execState (tokenize parseTokens str) Start in v
{-# NOINLINE parse #-}
