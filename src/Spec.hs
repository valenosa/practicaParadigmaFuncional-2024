module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "2 " $ do
      golpe bart putter `shouldBe` UnTiro {velocidad = 10, precision = 120, altura = 0}
    it "4b" $ do
      cuantosObstaculos (UnTiro 10 95 0) [tunelConRampita, tunelConRampita, hoyo] `shouldBe` 2
