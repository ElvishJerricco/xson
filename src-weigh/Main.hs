{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import           Data.Aeson
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as Lazy
import           Data.Monoid          ((<>))
import           Data.Xson
import           Weigh

main :: IO ()
main = do
  aer     <- ByteString.readFile "AER-x.json"
  allSets <- ByteString.readFile "AllSetsArray-x.json"
  let slash = "\"" <> ByteString.pack (replicate 100000 escapeSlash) <> "\""
  mainWith $ do
    func "xson-aer"        parse           aer
    func "xson-aer-ST"     parseST         aer
    func "aeson-aer"       (decode @Value) (Lazy.fromStrict aer)
    func "xson-allSets"    parse           allSets
    func "xson-allSets-ST" parseST         allSets
    func "aeson-allSets"   (decode @Value) (Lazy.fromStrict allSets)
    func "xson-slash"      parse           slash
    func "xson-slash-ST"   parseST         slash
    func "aeson-slash"     (decode @Value) (Lazy.fromStrict slash)
