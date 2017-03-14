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

data TState
  = UnexpectedToken
  | NoClosingQuote
  | MidNumber Scientific

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
minusChar = S.index "-" 0

period :: Word8
period = S.index "." 0

doubleQuote :: Word8
doubleQuote = S.index "\"" 0

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

parseWithStateMachine :: Monad m => (Token -> m ()) -> m PState -> L.ByteString -> m (Maybe Value)
parseWithStateMachine next current str = do
  let op s x (startWith, _) = do
        let s' = startWith <> s
        (i, tstate) <- tokenize next s'
        x (S.drop i s', tstate)
  (_, tstate) <- L.foldrChunks op return str ("", Nothing)
  case tstate of
    Nothing              -> do
      pstate <- current
      case pstate of
        Done v -> return (Just v)
        _      -> return Nothing
    Just (MidNumber sci) -> do
      next (Number sci)
      pstate <- current
      case pstate of
        Done v -> return (Just v)
        _      -> return Nothing
    Just _               -> return Nothing

parse :: L.ByteString -> Maybe Value
parse str = flip evalState Start $ parseWithStateMachine (modify' . nextPState) get str

parseST :: L.ByteString -> Maybe Value
parseST str = runST $ do
  ref <- newSTRef Start
  parseWithStateMachine (modifySTRef' ref . nextPState) (readSTRef ref) str
