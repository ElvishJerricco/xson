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
  let slash = "\"" <> L.pack (replicate 100000 escapeSlash) <> "\""
  defaultMain
    [ bench "xson-aer"        (nfIO $ fmap parse (L.readFile "AER-x.json"))
    , bench "xson-aer-ST"     (nfIO $ fmap parseST (L.readFile "AER-x.json"))
    , bench "aeson-aer"       (nfIO $ fmap (decode @Value) (L.readFile "AER-x.json"))
    , bench "xson-allSets"    (nfIO $ fmap parse (L.readFile "AllSetsArray-x.json"))
    , bench "xson-allSets-ST" (nfIO $ fmap parseST (L.readFile "AllSetsArray-x.json"))
    , bench "aeson-allSets"   (nfIO $ fmap (decode @Value) (L.readFile "AllSetsArray-x.json"))
    , bench "xson-slash"      (nf parse slash)
    , bench "xson-slash-ST"   (nf parseST slash)
    , bench "aeson-slash"     (nf (decode @Value) slash)
    ]
