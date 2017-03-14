{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Applicative
import           Control.Lens.Indexed
import           Control.Monad
import           Data.Aeson
import           Data.Align
import qualified Data.ByteString.Lazy as ByteString
import           Data.Maybe
import           Data.These
import           Data.Xson (parse, parseST)
import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck

main :: IO ()
main = do
  defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests = [testGroup "QuickCheck" qcTests, testGroup "Aeson checks" huTests]

qcTests :: [TestTree]
qcTests = [] -- TODO

huTests :: [TestTree]
huTests =
  [ testCase "AER"         (aesonCheck "AER-x.json")
  , testCase "All Sets"    (aesonCheck "AllSetsArray-x.json")
  , testCase "AER ST"      (aesonCheckST "AER-x.json")
  , testCase "All Sets ST" (aesonCheckST "AllSetsArray-x.json")
  ]

aesonCheck :: FilePath -> Assertion
aesonCheck path = do
  xsonStr  <- ByteString.readFile path
  aesonStr <- ByteString.readFile path
  -- use 'intercalate "\n"' instead of 'fromMaybe ""' to see all diffs
  assertString $ fromMaybe "" $ valueDiffMsg (parse xsonStr) (decode aesonStr)

aesonCheckST :: FilePath -> Assertion
aesonCheckST path = do
  xsonStr  <- ByteString.readFile path
  aesonStr <- ByteString.readFile path
  -- use 'intercalate "\n"' instead of 'fromMaybe ""' to see all diffs
  assertString $ fromMaybe "" $ valueDiffMsg (parseST xsonStr) (decode aesonStr)

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
      f i (These a b) = fmap ((replicate tab ' ' ++ "At: " ++ show i ++ "\n")++) (go (tab + 1) a b)
      f i (This _   ) = pure $ "Index [" ++ show i ++ "] not present in RHS"
      f i (That _   ) = pure $ "Index [" ++ show i ++ "] not present in LHS"
    in
      ifoldr (\i a acc -> acc <|> f i a) empty (align x y)
