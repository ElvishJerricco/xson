{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Xson.Conduit.FromJSON where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.Aeson (Value)
import qualified Data.ByteString.Lazy as L
import           Data.Conduit
import           Data.Functor.Identity
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Xson

class FromJSON a where
  parseTokens :: Monad m => Consumer Token m (Maybe a)

instance FromJSON Scientific where
  parseTokens = runMaybeT $ do
    t <- MaybeT await
    case t of
      Number sci -> return sci
      _ -> empty
  {-# INLINE parseTokens #-}

instance FromJSON Bool where
  parseTokens = runMaybeT $ do
    t <- MaybeT await
    case t of
      Boolean b -> return b
      _ -> empty
  {-# INLINE parseTokens #-}

instance FromJSON Text where
  parseTokens = runMaybeT $ do
    t <- MaybeT await
    case t of
      String (T.decodeUtf8' -> Right s) -> return s
      _ -> empty
  {-# INLINE parseTokens #-}

instance FromJSON a => FromJSON [a] where
  parseTokens = runMaybeT $ do
    t <- MaybeT await
    case t of
      OpenArray -> loop id
      _ -> empty
   where
    loop f = do
      t <- MaybeT await
      case t of
        CloseArray -> return (f [])
        _ -> do
          let yielder = do
                yield t
                yielderLoop
              yielderLoop = do
                x <- await
                maybe (return ()) yield x
          a <- MaybeT $ yielder .| parseTokens
          loop (f . (a:))
  {-# INLINE parseTokens #-}

instance FromJSON a => FromJSON (Vector a) where
  parseTokens = fmap (fmap Vector.fromList) parseTokens
  {-# INLINE parseTokens #-}

instance FromJSON a => FromJSON (HashMap Text a) where
  parseTokens = runMaybeT $ do
    t <- MaybeT await
    case t of
      OpenObject -> loop Map.empty
      _ -> empty
   where
    loop as = do
      t <- MaybeT await
      case t of
        CloseObject -> return as
        String (T.decodeUtf8' -> Right s) -> do
          a <- MaybeT parseTokens
          loop $ Map.insert s a as
        _ -> empty
  {-# INLINE parseTokens #-}

instance FromJSON Value where
  parseTokens = evalStateT (runMaybeT loop) Start
   where
    loop = do
      t <- MaybeT $ lift await
      modify' (nextPState t)
      pstate <- get
      case pstate of
        Done v -> return v
        PError _ -> empty
        _ -> loop
  {-# INLINE parseTokens #-}

decode :: FromJSON a => L.ByteString -> Maybe a
decode str = runIdentity $ (() <$ parseWithStateMachine yield str) $$ parseTokens
{-# INLINABLE decode #-}
{-# SPECIALIZE decode :: L.ByteString -> Maybe Value #-}
