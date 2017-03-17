{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           Criterion
import           Criterion.Main
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Monoid ((<>))
import           Data.Xson

main :: IO ()
main = do
  let slash   = "\"" <> L.pack (replicate 100000 escapeSlash) <> "\""
      aer     = L.readFile "AER-x.json"
      allSets = L.readFile "AllSetsArray-x.json"
  defaultMain
    [ bench "xson-aer"        (nfIO $ fmap parse aer)
    , bench "xson-aer-ST"     (nfIO $ fmap parseST aer)
    , bench "aeson-aer"       (nfIO $ fmap (decode @Value) aer)
    , bench "xson-allSets"    (nfIO $ fmap parse allSets)
    , bench "xson-allSets-ST" (nfIO $ fmap parseST allSets)
    , bench "aeson-allSets"   (nfIO $ fmap (decode @Value) allSets)
    , bench "xson-slash"      (nf parse slash)
    , bench "xson-slash-ST"   (nf parseST slash)
    , bench "aeson-slash"     (nf (decode @Value) slash)
    ]
