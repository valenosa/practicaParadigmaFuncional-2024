module Library where
import PdePreludat

type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre


esVocal :: Char -> Bool
esVocal = flip elem "aeiou"


tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"




cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

--1)
--a)
rimaCon :: Palabra -> Palabra -> Bool
rimaCon palabra1 palabra2 = puedeSerRima palabra1 palabra2 && (rimaAsonante palabra1 palabra2 || rimaConsonante palabra1 palabra2)

puedeSerRima :: Palabra -> Palabra -> Bool
puedeSerRima = (/=)

rimaAsonante :: Palabra -> Palabra -> Bool
rimaAsonante = cumplen (ultimas 2.vocales) (==)

rimaConsonante :: Palabra -> Palabra -> Bool
rimaConsonante = cumplen (ultimas 3) (==)

ultimas :: Number -> String -> String
ultimas n = take n . reverse

vocales :: Palabra -> String
vocales = filter esVocal 

--b) 
-- No riman porque son iguales
-- No riman porque las últimas tres letras y las últimas dos vocales son distintas
-- Riman porque las últimas tres letras son iguales (y las palabras distintas)
-- Riman porque las últimas dos vocales son iguales (y las palabras distintas)

-- 2)

conjuegan :: (Verso, Verso) -> (Verso -> Verso -> Bool) -> Bool
conjuegan (verso1, verso2)  criterio = criterio verso1 verso2

porMedioDeAnadiplosis :: Verso -> Verso -> Bool
porMedioDeAnadiplosis verso1 verso2 = (ultima `palabraDe` verso1) == primer `palabraDe` verso2

palabraDe :: ([String] -> String) -> String -> String
palabraDe posicion verso = posicion (words verso)
ultima = last

primer = head

porMedioDeRimas :: Verso -> Verso -> Bool
porMedioDeRimas verso1 verso2 = (ultima `palabraDe` verso1) `rimaCon` (ultima `palabraDe` verso2)

-- 3)

type Patron = Estrofa -> Bool

-- hayPatron :: Estrofa -> Patron -> Bool
-- hayPatron verso1 verso2 patron = patron verso1 verso2

simple :: Number -> Number -> Patron 
simple n m estrofa = ((n-1) `versoDeLa` estrofa, (m-1) `versoDeLa` estrofa) `conjuegan` porMedioDeRimas

versoDeLa :: Number -> Estrofa -> Verso
versoDeLa n = (!! n)

--FALTA PROBAR
esdrujula :: Patron
esdrujula = all versoTerminaConEsdrujula 

versoTerminaConEsdrujula :: Verso -> Bool
versoTerminaConEsdrujula = esEsdrujula.last.words

esEsdrujula :: Palabra -> Bool
esEsdrujula  = hayTildeEnLaAntepenultimaVocal.take 3.reverse.filter (\letra -> esVocal letra || tieneTilde letra)

hayTildeEnLaAntepenultimaVocal :: Palabra -> Bool
hayTildeEnLaAntepenultimaVocal [x, y, z] =  tieneTilde z
hayTildeEnLaAntepenultimaVocal _ =  False