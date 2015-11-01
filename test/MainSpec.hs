module MainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import ParseUtils (extractId)
import StationParsers (extractStationName)

spec :: Spec
spec = do
  describe "ParseUtils" $ do
    context "extractId" $ do
      it "extracts id from a properly formatted query string" $ do
        extractId "?id=123" `shouldBe` 123
      
      -- |Lol, this shouldn't happen :D
      it "behaves properly on an improperly formatted string" $ do
        extractId "lollo" `shouldBe` -1

      it "behaves properly when no id parameter is supplied" $ do
        extractId "?something=else" `shouldBe` -1

      it "behaves properly with empty id parameter" $ do
        extractId "?something=foo&id=" `shouldBe` -1

    context "extractStationName" $ do
      it "should extract station name" $ do
        extractStationName "rozkład stacji Kraków " `shouldBe` "Kraków"

