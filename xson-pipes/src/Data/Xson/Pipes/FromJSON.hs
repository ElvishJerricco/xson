{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Xson.Pipes.FromJSON where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.Aeson (Value)
import qualified Data.ByteString.Lazy as L
import           Data.Functor.Identity
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Xson
import           Pipes

class FromJSON a where
  parseTokens :: Monad m => Consumer' Token m (Maybe a)

instance FromJSON Scientific where
  parseTokens = do
    t <- await
    return $ case t of
      Number sci -> Just sci
      _ -> Nothing
  {-# INLINE parseTokens #-}

instance FromJSON Bool where
  parseTokens = do
    t <- await
    return $ case t of
      Boolean b -> Just b
      _ -> Nothing
  {-# INLINE parseTokens #-}

instance FromJSON Text where
  parseTokens = do
    t <- await
    return $ case t of
      String (T.decodeUtf8' -> Right s) -> Just s
      _ -> Nothing
  {-# INLINE parseTokens #-}

instance FromJSON a => FromJSON [a] where
  parseTokens = do
    t <- await
    case t of
      OpenArray -> loop id
      _ -> return Nothing
   where
    loop f = do
      t <- await
      case t of
        CloseArray -> return (Just (f []))
        _ -> do
          let yielder = do
                yield t
                forever $ yield =<< await
          a <- yielder >-> parseTokens
          case a of
            Nothing -> return Nothing
            Just a' -> loop (f . (a':))
  {-# INLINE parseTokens #-}

instance FromJSON a => FromJSON (Vector a) where
  parseTokens = fmap (fmap Vector.fromList) parseTokens
  {-# INLINE parseTokens #-}

instance FromJSON a => FromJSON (HashMap Text a) where
  parseTokens = do
    t <- await
    case t of
      OpenObject -> loop Map.empty
      _ -> return Nothing
   where
    loop as = do
      t <- await
      runMaybeT $ case t of
        CloseObject -> return as
        String (T.decodeUtf8' -> Right s) -> do
          a <- MaybeT parseTokens
          MaybeT $ loop $ Map.insert s a as
        _ -> empty
  {-# INLINE parseTokens #-}

-- instance FromJSON Value where
--   parseTokens = do
--     t <- await
--     let yielder = do
--           yield t
--           forever $ yield =<< await
--     case t of
--       OpenObject -> fmap Aeson.Object <$> (yielder >-> parseTokens)
--       OpenArray -> fmap Aeson.Array <$> (yielder >-> parseTokens)
--       String (T.decodeUtf8' -> Right s) -> return (Just (Aeson.String s))
--       Number sci -> return (Just (Aeson.Number sci))
--       Boolean b -> return (Just (Aeson.Bool b))
--       Null -> return (Just Aeson.Null)
--       _ -> return Nothing
--   {-# INLINE parseTokens #-}

instance FromJSON Value where
  parseTokens = evalStateT loop Start
   where
    loop = do
      t <- lift await
      modify' (nextPState t)
      pstate <- get
      case pstate of
        Done v -> return (Just v)
        PError _ -> return Nothing
        _ -> loop
  {-# INLINE parseTokens #-}

decode :: FromJSON a => L.ByteString -> Maybe a
decode str = runIdentity $ runEffect $ (Nothing <$ parseWithStateMachine yield str) >-> parseTokens
{-# INLINABLE decode #-}
{-# SPECIALIZE decode :: L.ByteString -> Maybe Value #-}
