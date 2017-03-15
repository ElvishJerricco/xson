{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Monoid ((<>))
import           Data.Xson
import qualified Data.Xson.FromJSON as X
import           Weigh

main :: IO ()
main = do
  let slash = "\"" <> L.pack (replicate 100000 escapeSlash) <> "\""
  mainWith $ do
    io   "xson-aer"              (fmap parse . L.readFile)             "AER-x.json"
    io   "xson-aer-ST"           (fmap parseST . L.readFile)           "AER-x.json"
    io   "xson-aer-FromJSON"     (fmap (X.decode @Value) . L.readFile) "AER-x.json"
    io   "aeson-aer"             (fmap (decode @Value) . L.readFile)   "AER-x.json"
    io   "xson-allSets"          (fmap parse . L.readFile)             "AllSetsArray-x.json"
    io   "xson-allSets-ST"       (fmap parseST . L.readFile)           "AllSetsArray-x.json"
    io   "xson-allSets-FromJSON" (fmap (X.decode @Value) . L.readFile) "AllSetsArray-x.json"
    io   "aeson-allSets"         (fmap (decode @Value) . L.readFile)   "AllSetsArray-x.json"
    func "xson-slash"            parse                                 slash
    func "xson-slash-ST"         parseST                               slash
    func "xson-slash-FromJSON"   (X.decode @Value)                     slash
    func "aeson-slash"           (decode @Value)                       slash
