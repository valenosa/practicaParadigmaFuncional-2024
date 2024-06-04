module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "quickSort funciona" $ do
      quickSort (>) [3,8,7,20,2,1] `shouldBe` [20,8,7,3,2,1]
    it "1 " $ do
      mayorSegun length bosqueTenebroso pantanoDelDestino `shouldBe` True
    it "2a" $ do
      tiempo amarillo (head bosqueTenebroso) `shouldBe` 16
    it "2b" $ do
      tiempoTotal bosqueTenebroso amarillo `shouldBe` 150
    it "3 " $ do
      podio bosqueTenebroso apocalipsis `shouldBe` [("Gise",(2,3,6)),("Mati",(4,4,4)),("Alf",(3,3,4))]
    it "4a" $ do
      elMejorDelTramo (head bosqueTenebroso) apocalipsis `shouldBe` "Gise"
    it "4b" $ do
      elMasWinner pantanoDelDestino apocalipsis `shouldBe` "Leo"
    it "5 " $ do
      quienesPueden (head bosqueTenebroso) 12 apocalipsis `shouldBe` ["Gise","Mati","Alf"]
    it "6 " $ do
      estadisticas bosqueTenebroso apocalipsis `shouldBe` [("Leo",0,150),("Gise",3,85),("Mati",2,138),("Alf",0,141)]
    it "7 " $ do
      fuePareja bosqueTenebroso apocalipsis `shouldBe` False
    