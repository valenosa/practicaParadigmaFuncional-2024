module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "rima asonante" $ do
      ("parcial" `rimaCon` "estirar") `shouldBe` True
    it "rima consonante" $ do
      ("función" `rimaCon` "canción") `shouldBe` True
    it "no riman (iguales)" $ do
      ("función" `rimaCon` "función") `shouldBe` False
    it "no riman" $ do
      ("perro" `rimaCon` "gato") `shouldBe` False
    it "conjuegan por medio de rimas" $ do
      (("no hace falta un programa que genere una canción", "para saber que esto se resuelve con una función") `conjuegan` porMedioDeRimas) `shouldBe` True
    it "conjuegan por medio de anadiplosis" $ do
      (("este examen no se aprueba sin aplicación parcial", "parcial lindo y divertido si rendiste todas las katas") `conjuegan` porMedioDeAnadiplosis) `shouldBe` True
    it "simple 1 4" $ do
      simple 1 4 ["esta rima es fácil como patear un penal", "solamente tiene como objetivo servir de ejemplo", "los versos del medio son medio fríos", "porque el remate se retoma al final"] `shouldBe` True
    it "simple 1 3" $ do
      simple 1 3 ["esta rima es fácil como patear un penal", "solamente tiene como objetivo servir de ejemplo", "los versos del medio son medio fríos", "porque el remate se retoma al final"] `shouldBe` False