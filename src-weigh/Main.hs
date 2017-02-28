{-# LANGUAGE TypeApplications #-}

import           Data.Aeson
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as Lazy
import           Lib
import           Weigh

main :: IO ()
main = do
  aer     <- ByteString.readFile "AER-x.json"
  allSets <- ByteString.readFile "AllSetsArray-x.json"
  mainWith $ do
    func "xson-aer"        parse           aer
    func "xson-allSets"    parse           allSets
    func "xson-aer-ST"     parseST         aer
    func "xson-allSets-ST" parseST         allSets
    func "aeson-aer"       (decode @Value) (Lazy.fromStrict aer)
    func "aeson-allSets"   (decode @Value) (Lazy.fromStrict allSets)
