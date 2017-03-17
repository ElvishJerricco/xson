{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Xson.FromJSON where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Aeson (Value)
import qualified Data.Aeson as Aeson
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
      String s -> either (const Nothing) Just $ T.decodeUtf8' s
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
        String s -> do
          a <- MaybeT parseTokens
          s' <- either (const empty) pure $ T.decodeUtf8' s
          MaybeT $ loop $ Map.insert s' a as
        _ -> empty
  {-# INLINE parseTokens #-}

instance FromJSON Value where
  parseTokens = do
    t <- await
    let yielder = do
          yield t
          forever $ yield =<< await
    case t of
      OpenObject -> fmap Aeson.Object <$> (yielder >-> parseTokens)
      OpenArray -> fmap Aeson.Array <$> (yielder >-> parseTokens)
      String s -> return $ Aeson.String <$> either (const Nothing) Just (T.decodeUtf8' s)
      Number sci -> return (Just (Aeson.Number sci))
      Boolean b -> return (Just (Aeson.Bool b))
      Null -> return (Just Aeson.Null)
      _ -> return Nothing
  {-# INLINE parseTokens #-}

decode :: FromJSON a => L.ByteString -> Maybe a
decode str = runIdentity $ runEffect $ (Nothing <$ parseWithStateMachine yield str) >-> parseTokens
{-# INLINABLE decode #-}
{-# SPECIALIZE decode :: L.ByteString -> Maybe Value #-}
