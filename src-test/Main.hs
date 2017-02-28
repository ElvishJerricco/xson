{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Applicative
import           Control.Lens.Indexed
import           Control.Monad
import           Data.Aeson
import           Data.Align
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as Lazy
import           Data.Maybe
import           Data.These
import           Lib                  (parse)
import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck

main :: IO ()
main = do
  aer     <- ByteString.readFile "AER-x.json"
  allSets <- ByteString.readFile "AllSetsArray-x.json"
  defaultMain $ testGroup "all-tests" (tests aer allSets)

tests :: ByteString -> ByteString -> [TestTree]
tests aer allSets =
  [ testGroup "QuickCheck"   qcTests
  , testGroup "Aeson checks" (huTests aer allSets)
  ]

qcTests :: [TestTree]
qcTests = [] -- TODO

huTests :: ByteString -> ByteString -> [TestTree]
huTests aer allSets =
  [testCase "AER" (aesonCheck aer), testCase "All Sets" (aesonCheck allSets)]

aesonCheck :: ByteString -> Assertion
aesonCheck str =
  assertString
    $ fromMaybe "" -- use 'intercalate "\n"' instead to see all diffs
    $ valueDiffMsg (Just (parse str)) (decode (Lazy.fromStrict str))

--------------------------------------------------------------------------------
-- Utils

-- | Construct a diff of two parsed values. There can be many diffs
-- along the way. Your choice of 'f' will determine how to choose
-- which diffs to display. Using 'Maybe' will show only the first.
-- Using '[]' will show all diffs.
valueDiffMsg :: Alternative f => Maybe Value -> Maybe Value -> f String
valueDiffMsg Nothing    Nothing    = empty
valueDiffMsg Nothing    _          = pure "LHS failed to parse"
valueDiffMsg _          Nothing    = pure "RHS failed to parse"
valueDiffMsg (Just lhs) (Just rhs) = go 0 lhs rhs
 where
  go tab (Object x) (Object y) = diffAlign tab x y
  go tab (Array  x) (Array  y) = diffAlign tab x y
  go tab (String x) (String y) = showDiff tab x y
  go tab (Bool   x) (Bool   y) = showDiff tab x y
  go tab (Number x) (Number y) = showDiff tab x y
  go _   Null       Null       = empty
  go _   _          _          = pure "Type mismatch"

  showDiff :: (Eq a, Show a, Alternative f) => Int -> a -> a -> f String
  showDiff tab x y =
    let t   = replicate tab ' '
        t1  = replicate (tab + 1) ' '
        msg = [t, "Value mismatch\n", t1, show x, "\n", t1, show y, "\n"]
    in  guard (x /= y) *> pure (mconcat msg)

  diffAlign
    :: (Align t, FoldableWithIndex i t, Show i, Alternative f)
    => Int
    -> t Value
    -> t Value
    -> f String
  diffAlign tab x y =
    let
      f i (These a b)
        = fmap ((replicate tab ' ' ++ "At: " ++ show i ++ "\n")++)
               (go (tab + 1) a b)
      f i (This _) = pure $ "Index [" ++ show i ++ "] not present in RHS"
      f i (That _) = pure $ "Index [" ++ show i ++ "] not present in LHS"
    in
      ifoldr (\i a acc -> acc <|> f i a) empty (align x y)
