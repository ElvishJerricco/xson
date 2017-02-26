{-# LANGUAGE TypeApplications #-}

import Weigh
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import Lib
import Data.Aeson

main :: IO ()
main = do
  aer <- ByteString.readFile "AER-x.json"
  allSets <- ByteString.readFile "AllSetsArray-x.json"
  mainWith $ do
    func "xson-aer" tokenize' aer
    func "xson-allSets" tokenize' allSets
    func "aeson-aer" (decode @Value) (Lazy.fromStrict aer)
    func "aeson-allSets" (decode @Value) (Lazy.fromStrict allSets)
