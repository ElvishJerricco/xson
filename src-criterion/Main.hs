{-# LANGUAGE TypeApplications #-}

import Criterion
import Criterion.Main
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import Lib
import Data.Aeson

main :: IO ()
main = do
  aer <- ByteString.readFile "AER-x.json"
  allSets <- ByteString.readFile "AllSetsArray-x.json"
  defaultMain
    [ bench "xson-aer" (whnf tokenize' aer)
    , bench "xson-allSets" (whnf tokenize' allSets)
    , bench "aeson-aer" (nf (decode @Value) (Lazy.fromStrict aer))
    , bench "aeson-allSets" (nf (decode @Value) (Lazy.fromStrict allSets))
    ]
