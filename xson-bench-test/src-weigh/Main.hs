{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Monoid ((<>))
import           Data.Xson
import qualified Data.Xson.Conduit.FromJSON as C
import qualified Data.Xson.Pipes.FromJSON as P
import           Weigh

main :: IO ()
main = do
  let slash = "\"" <> L.pack (replicate 100000 escapeSlash) <> "\""
  mainWith $ do
    io   "xson-aer"             (fmap parse . L.readFile)             "AER-x.json"
    io   "xson-aer-ST"          (fmap parseST . L.readFile)           "AER-x.json"
    io   "xson-aer-pipes"       (fmap (P.decode @Value) . L.readFile) "AER-x.json"
    io   "xson-aer-conduit"     (fmap (C.decode @Value) . L.readFile) "AER-x.json"
    io   "aeson-aer"            (fmap (decode @Value) . L.readFile)   "AER-x.json"
    io   "xson-allSets"         (fmap parse . L.readFile)             "AllSetsArray-x.json"
    io   "xson-allSets-ST"      (fmap parseST . L.readFile)           "AllSetsArray-x.json"
    io   "xson-allSets-pipes"   (fmap (P.decode @Value) . L.readFile) "AllSetsArray-x.json"
    io   "xson-allSets-conduit" (fmap (C.decode @Value) . L.readFile) "AllSetsArray-x.json"
    io   "aeson-allSets"        (fmap (decode @Value) . L.readFile)   "AllSetsArray-x.json"
    func "xson-slash"           parse                                 slash
    func "xson-slash-ST"        parseST                               slash
    func "xson-slash-pipes"     (P.decode @Value)                     slash
    func "xson-slash-conduit"   (C.decode @Value)                     slash
    func "aeson-slash"          (decode @Value)                       slash
