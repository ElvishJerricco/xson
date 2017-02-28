{-# LANGUAGE TypeApplications #-}

import           Criterion
import           Criterion.Main
import           Data.Aeson
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as Lazy
import           Lib

main :: IO ()
main = do
  aer     <- ByteString.readFile "AER-x.json"
  allSets <- ByteString.readFile "AllSetsArray-x.json"
  defaultMain
    [ bench "xson-aer"        (nf parse aer)
    , bench "xson-aer-ST"     (nf parseST aer)
    , bench "aeson-aer"       (nf (decode @Value) (Lazy.fromStrict aer))
    , bench "xson-allSets"    (nf parse allSets)
    , bench "xson-allSets-ST" (nf parseST allSets)
    , bench "aeson-allSets"   (nf (decode @Value) (Lazy.fromStrict allSets))
    ]
