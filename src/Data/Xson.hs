{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Xson where

import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import           Data.Monoid ((<>))
import           Data.STRef.Strict
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import           Data.Word

-- | A semantic token of JSON. Commas and colons are not included, as
-- they are semantically meaningless to the parser. Strings are
-- pre-escaped.
data Token
  = OpenObject
  | CloseObject
  | String ByteString
  | OpenArray
  | CloseArray
  | Number Scientific
  | Boolean Bool
  | Null
  deriving (Show, Ord, Eq)

-- | After tokenizing a chunk, the tokenizer is left in one of a few
-- states. Mostly, these are error states, possibly due to a token
-- crossing the chunk boundary. But it's impossible to tell whether
-- numbers that cross the chunk boundary are done or not, if you can't
-- see the next chunk. So we use a tokenizer state to indicate that
-- the number token might either be done, or crossing a chunk.
data TState
  = UnexpectedToken
  | NoClosingQuote
  | MidNumber Scientific

-- | This takes a chunk of JSON and spits out tokens to the callback
-- function. It operates on chunks of JSON, allowing the data to be
-- streamed in piece by piece, instead of all at once. If a token is
-- incomplete and crosses the chunk boundary, the index of the start
-- of that token is returned, and the relevant 'TState' is
-- returned. The parsing phase should append the bytes from that index
-- onward to the beginning of the next chunk, in order to get that
-- token correctly.
--
-- This tokenizer makes it possible to create your own parsing state
-- machine, bypassing the 'Value' intermediate value entirely. This
-- improve the performance drastically.
--
-- TODO: Currently string escaping and number parsing are the slowest
-- parts of this (they are the only reason the tokenizer doesn't run
-- with zero GCs). It'd be good to find an effective way to batch and
-- paralellize them. This might involve recognizing numbers into
-- bytestrings rather than parsing them in the tokenizer, and having
-- the parser parse them into actual numbers.
tokenize :: forall f . Applicative f => (Token -> f ()) -> ByteString -> f (Int, Maybe TState)
tokenize handleToken str = go 0
 where
  go (skipSpaces str -> index) = case S.uncons (S.drop index str) of
    -- TODO: Replace the prefix checks with manually unrolled loops.
    Just (x, _)
      | x == doubleQuote                        -> findClosingQuote index (index + 1)
      | x == openBrace                          -> handleToken OpenObject *> go (index + 1)
      | x == closeBrace                         -> handleToken CloseObject *> go (index + 1)
      | x == openBracket                        -> handleToken OpenArray *> go (index + 1)
      | x == closeBracket                       -> handleToken CloseArray *> go (index + 1)
      | x == minusChar                          -> number index negate (index + 1)
      | isNumberChar x                          -> number index id index
      | S.isPrefixOf "true" (S.drop index str)  -> handleToken (Boolean True) *> go (index + 4)
      | S.isPrefixOf "false" (S.drop index str) -> handleToken (Boolean False) *> go (index + 4)
      | S.isPrefixOf "null" (S.drop index str)  -> handleToken Null *> go (index + 4)
      | otherwise                               -> pure (index, Just UnexpectedToken)
    _ -> pure (index, Nothing)
  -- 'start' is the index of the opening quote.
  -- 'index' is the index to start searching at.
  -- 'index' advances with each escaped quote found.
  findClosingQuote start index = case elemIndexFrom doubleQuote str index of
    Nothing     -> pure (start, Just NoClosingQuote)
    Just index' -> if isEscaped index'
      then findClosingQuote start (index' + 1)
      else handleToken (String (escaped $ substring str (start + 1) index')) *> go (index' + 1)
  -- TODO: Maybe make this tail call itself?
  isEscaped index =
    let x = S.index str (index - 1) in x == escapeSlash && not (isEscaped (index - 1))
  number :: Int -> (Integer -> Integer) -> Int -> f (Int, Maybe TState)
  number start sign index =
    let (int, index') = parseNumber str index in decimal start sign int index'
  decimal :: Int -> (Integer -> Integer) -> Integer -> Int -> f (Int, Maybe TState)
  decimal start sign int index = case S.uncons (S.drop index str) of
    Just (x, _) | x == period ->
      let (dec, index') = parseNumber str (index + 1)
          e             = index' - (index + 1)
      in  expon start (sign (int * 10 ^ e + dec)) (-e) index'
    _ -> expon start (sign int) 0 index
  expon :: Int -> Integer -> Int -> Int -> f (Int, Maybe TState)
  expon start n e index =
    let (sci, index') = case S.uncons (S.drop index str) of
          Just (x, _) | isExponent x ->
            let (e', i) = parseNumber str (index + 1) :: (Int, Int) in (scientific n (e + e'), i)
          _ -> (scientific n e, index)
    in  if index' == S.length str
          then pure (start, Just $ MidNumber sci)
          else handleToken (Number sci) *> go index'
{-# INLINE tokenize #-}
{-# SPECIALIZE tokenize :: (Token -> State s ()) -> ByteString -> State s (Int, Maybe TState) #-}
{-# SPECIALIZE tokenize :: (Token -> ST s ()) -> ByteString -> ST s (Int, Maybe TState) #-}

--------------------------------------------------------------------------------
-- ByteString utilities

-- | A fast space skipping function.
skipSpaces :: ByteString -> Int -> Int
skipSpaces str i = case S.uncons (S.drop i str) of
  Just (x, _) | isSpaceChar x -> skipSpaces str (i + 1)
  _                           -> i
{-# INLINE skipSpaces #-}

-- | Get a substring of a string.
substring :: ByteString -> Int -> Int -> ByteString
substring s start end = S.take (end - start) (S.drop start s)
{-# INLINE substring #-}

-- TODO: Definitely look into this:
-- http://www.serpentine.com/blog/2015/05/13/sometimes-the-old-ways-are-the-best/
escaped :: ByteString -> ByteString
escaped str = case S.elemIndex escapeSlash str of
  Nothing -> str
  Just index ->
    let preStr = substring str 0 index
    in  case S.uncons (S.drop (index + 1) str) of
          Just (x, str')
            | x == doubleQuote  -> preStr `S.append` (doubleQuote `S.cons` escaped str')
            | x == escapeSlash  -> preStr `S.append` (escapeSlash `S.cons` escaped str')
            | x == forwardSlash -> preStr `S.append` (forwardSlash `S.cons` escaped str')
            | x == bChar        -> preStr `S.append` (backspace `S.cons` escaped str')
            | x == fChar        -> preStr `S.append` (formFeed `S.cons` escaped str')
            | x == nChar        -> preStr `S.append` (newLine `S.cons` escaped str')
            | x == rChar        -> preStr `S.append` (carriageReturn `S.cons` escaped str')
            | x == tChar        -> preStr `S.append` (tabChar `S.cons` escaped str')
            | x == uChar        -> error "TODO: Implement unicode escapes"
            | otherwise         -> preStr `S.append` (x `S.cons` escaped str')
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
-- carriage return, and tab. However, commas and colons can be treated
-- as whitespace if your parser recognizes as such.
isSpaceChar :: Word8 -> Bool
isSpaceChar c = c == 0x20 || c == 0x0a || c == 0x0d || c == 0x09 || c == 0x2C || c == 0x3A
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
minusChar = 0x2D
{-# INLINE minusChar #-}

period :: Word8
period = 0x2E
{-# INLINE period #-}

doubleQuote :: Word8
doubleQuote = 0x22
{-# INLINE doubleQuote #-}

escapeSlash :: Word8
escapeSlash = 0x5C
{-# INLINE escapeSlash #-}

openBracket :: Word8
openBracket = 0x5B
{-# INLINE openBracket #-}

closeBracket :: Word8
closeBracket = 0x5D
{-# INLINE closeBracket #-}

openBrace :: Word8
openBrace = 0x7B
{-# INLINE openBrace #-}

closeBrace :: Word8
closeBrace = 0x7D
{-# INLINE closeBrace #-}

forwardSlash :: Word8
forwardSlash = 0x2F
{-# INLINE forwardSlash #-}

bChar :: Word8
bChar = 0x62
{-# INLINE bChar #-}

backspace :: Word8
backspace = 0x08
{-# INLINE backspace #-}

fChar :: Word8
fChar = 0x66
{-# INLINE fChar #-}

formFeed :: Word8
formFeed = 0x0C
{-# INLINE formFeed #-}

nChar :: Word8
nChar = 0x6E
{-# INLINE nChar #-}

newLine :: Word8
newLine = 0x0A
{-# INLINE newLine #-}

rChar :: Word8
rChar = 0x72
{-# INLINE rChar #-}

carriageReturn :: Word8
carriageReturn = 0x0D
{-# INLINE carriageReturn #-}

tChar :: Word8
tChar = 0x74
{-# INLINE tChar #-}

tabChar :: Word8
tabChar = 0x09
{-# INLINE tabChar #-}

uChar :: Word8
uChar = 0x75
{-# INLINE uChar #-}

--------------------------------------------------------------------------------
-- A state machine for parsing 'Value'

type ObjectDone = HashMap Text Value -> PState

data ObjectState
  = ObjectKey
  | ObjectValue !Text

type ArrayDone = [Value] -> PState

data PState
  = Start
  | Done !Value
  | PArray !ArrayDone
  | PObject !ObjectState !ObjectDone
  | PError !Text

nextPState :: Token -> PState -> PState
nextPState tok Start                            = startPState tok
nextPState _   (Done   _                      ) = PError "Done"
nextPState tok (PArray done                   ) = nextArrayValue done tok
nextPState tok (PObject ObjectKey         done) = nextObjectKey done tok
nextPState tok (PObject (ObjectValue key) done) = nextObjectValue done key tok
nextPState _   (PError err                    ) = PError err

startPState :: Token -> PState
startPState OpenArray           = PArray (Done . doneArray)
startPState OpenObject          = PObject ObjectKey (Done . Aeson.Object)
startPState (primVal -> Just v) = Done v
startPState _                   = PError "Expected value"

nextArrayValue :: ArrayDone -> Token -> PState
nextArrayValue done OpenArray           = PArray $ \v -> PArray (done . (doneArray v:))
nextArrayValue done OpenObject = PObject ObjectKey $ \v -> PArray (done . (Aeson.Object v:))
nextArrayValue done CloseArray          = done []
nextArrayValue done (primVal -> Just v) = PArray (done . (v:))
nextArrayValue _    _                   = PError "Expected value"

nextObjectKey :: ObjectDone -> Token -> PState
nextObjectKey done (String str) = PObject (ObjectValue (Text.decodeUtf8 str)) done
nextObjectKey done CloseObject  = done HashMap.empty
nextObjectKey _    _            = PError "Expected object key or close"

nextObjectValue :: ObjectDone -> Text -> Token -> PState
nextObjectValue done key OpenArray =
  PArray $ \v -> PObject ObjectKey (done . HashMap.insert key (doneArray v))
nextObjectValue done key OpenObject =
  PObject ObjectKey $ \v -> PObject ObjectKey (done . HashMap.insert key (Aeson.Object v))
nextObjectValue done key (primVal -> Just v) = PObject ObjectKey (done . HashMap.insert key v)
nextObjectValue _    _   _                   = PError "Expected value"

primVal :: Token -> Maybe Value
primVal (String  str) = Just (Aeson.String (Text.decodeUtf8 str))
primVal (Number  n  ) = Just (Aeson.Number n)
primVal (Boolean b  ) = Just (Aeson.Bool b)
primVal Null          = Just Aeson.Null
primVal _             = Nothing

doneArray :: [Value] -> Value
doneArray = Aeson.Array . Vector.fromList

--------------------------------------------------------------------------------
-- Parsers

-- | Parse a 'Value' using a state machine. Will return the
-- tokenizer's state after the last chunk. If the tokenizer's state is
-- 'MidNumber', this is not returned, and a 'Number' token is
-- propagated instead.
parseWithStateMachine :: Monad m => (Token -> m ()) -> L.ByteString -> m (Maybe TState)
parseWithStateMachine next str = do
  let op s x (startWith, _) = do
        let s' = startWith <> s
        (i, tstate) <- tokenize next s'
        x (S.drop i s', tstate)
  (_, tstate) <- L.foldrChunks op return str ("", Nothing)
  case tstate of
    Just (MidNumber sci) -> do
      next (Number sci)
      return Nothing
    _                    -> return tstate
{-# INLINE parseWithStateMachine #-}

-- | Parse a 'Value' using the 'State' based state machine. A lazy
-- bytestring is used as input so that chunks of input can be streamed
-- in rather than loading the entire input up front.
parse :: L.ByteString -> Maybe Value
parse str = flip evalState Start $ do
  tstate <- parseWithStateMachine (modify' . nextPState) str
  pstate <- get
  case tstate of
    Just _  -> return Nothing
    Nothing -> case pstate of
      Done v -> return (Just v)
      _      -> return Nothing

-- | Parse a 'Value' using the 'ST' based state machine. A lazy
-- bytestring is used as input so that chunks of input can be streamed
-- in rather than loading the entire input up front.
parseST :: L.ByteString -> Maybe Value
parseST str = runST $ do
  ref    <- newSTRef Start
  tstate <- parseWithStateMachine (modifySTRef' ref . nextPState) str
  pstate <- readSTRef ref
  case tstate of
    Just _  -> return Nothing
    Nothing -> case pstate of
      Done v -> return (Just v)
      _      -> return Nothing
