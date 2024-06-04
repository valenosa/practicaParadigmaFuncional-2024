module Library where
import PdePreludat




--Pistas
type Tramo = (Number, Chocobo -> Number)
type Pista = [Tramo]

f1 :: Chocobo -> Number
f1 chocobo = velocidad chocobo * 2
f2 :: Chocobo -> Number
f2 chocobo = velocidad chocobo + fuerza chocobo
f3 :: Chocobo -> Number
f3 chocobo = velocidad chocobo / peso chocobo

bosqueTenebroso :: Pista
bosqueTenebroso = [(100, f1), (50, f2), (120, f2), (200, f1), (80, f3)]

pantanoDelDestino :: Pista
pantanoDelDestino = [(40, f2), (90, \(f,p,v)-> f + p + v), (120, fuerza), (20, fuerza)]


-- Chocobos
type Chocobo = (Number, Number, Number)

amarillo :: Chocobo
amarillo = (5, 3, 3)

negro :: Chocobo
negro = (4, 4, 4)

blanco :: Chocobo
blanco = (2, 3, 6)

rojo :: Chocobo
rojo = (3, 4, 4)


--Funciones de acceso
fuerza :: Chocobo -> Number
fuerza (f,_,_) = f

peso :: Chocobo -> Number
peso (_,p,_) = p

velocidad :: Chocobo -> Number
velocidad (_,_,v) = v


--Jinetes
type Jinete = (String, Chocobo)

apocalipsis :: [Jinete]
apocalipsis = [("Leo", amarillo), ("Gise", blanco), ("Mati", negro), ("Alf",rojo)]

nombreJinete :: Jinete -> String
nombreJinete = fst

chocoboJinete :: Jinete -> Chocobo
chocoboJinete = snd


--quickSort
quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = []
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs


--Desarrollo:

--1)
-- Que pasa si es igual?

mayorSegun :: (a->Number) -> a -> a -> Bool
mayorSegun funcion valor1 valor2 = funcion valor1 > funcion valor2

menorSegun ::Eq a => (a -> Number) -> a -> a -> Bool
menorSegun funcion valor1 valor2
    |valor1 == valor2 = False
    |otherwise = not  (mayorSegun funcion valor1 valor2) && valor1 /= valor2


--2)
--a)
--El resultado no es entero, y la unica forma de que de 16 es redondearlo hacia abajo
tiempo :: Chocobo -> Tramo -> Number
tiempo chocobo (distancia, correccionDeVelocidad) = (floor.(distancia /).correccionDeVelocidad) chocobo

--b)
tiempoTotal :: Pista -> Chocobo -> Number
tiempoTotal pista chocobo = (sum.map (tiempo chocobo)) pista


--3)
puestosJinetesOrdenados :: (Jinete -> Number) -> [Jinete] -> [Jinete]
puestosJinetesOrdenados criterio = quickSort (menorSegun criterio)

puestosJinetesMenorAMayor :: Pista -> [Jinete] -> [Jinete]
puestosJinetesMenorAMayor pista = puestosJinetesOrdenados (tiempoTotal pista.chocoboJinete)

podio :: Pista -> [Jinete] -> [Jinete]
podio pista = take 3.puestosJinetesMenorAMayor pista


--4)
--a)
elMejorDelTramo :: Tramo -> [Jinete] -> String
elMejorDelTramo tramo = nombreJinete.head.puestosJinetesOrdenados ((`tiempo` tramo).chocoboJinete) --Tambien se puede usar (flip tiempo tramo) 

--b)
elMasWinner :: Pista -> [Jinete] -> String
elMasWinner pista jinetes = (nombreJinete.last.puestosJinetesOrdenados (cuantosTramosGanados pista jinetes)) jinetes

cuantosTramosGanados :: Pista -> [Jinete] -> Jinete -> Number
cuantosTramosGanados pista jinetes = length.tramosGanados pista jinetes

tramosGanados :: [Tramo] -> [Jinete] -> Jinete -> [Tramo]
tramosGanados pista jinetes jinete = filter (esElMejorDelTramo jinete jinetes) pista

esElMejorDelTramo :: Jinete -> [Jinete] -> Tramo -> Bool
esElMejorDelTramo jinete jinetes tramo = nombreJinete jinete == elMejorDelTramo tramo jinetes

-- Terminó el parcial

--5)
-- quienesPueden :: Tramo -> Number -> [Jinete] -> [String]
quienesPueden tramo tiempoEsperado = map nombreJinete.jinetesQuePueden tramo tiempoEsperado

jinetesQuePueden :: Tramo -> Number -> [Jinete] -> [Jinete]
jinetesQuePueden tramo tiempoEsperado = filter (puedeHacerlo tramo tiempoEsperado)

puedeHacerlo :: Tramo -> Number -> Jinete -> Bool
puedeHacerlo tramo tiempoEsperado jinete = tiempo (chocoboJinete jinete) tramo <=  tiempoEsperado

--6)
estadisticas :: Pista -> [Jinete] -> [(String, Number, Number)]
estadisticas pista jinetes = map (estadisticasJinete pista jinetes) jinetes

estadisticasJinete :: Pista -> [Jinete] -> Jinete -> (String, Number, Number)
estadisticasJinete pista jinetes jinete = (nombreJinete jinete, cuantosTramosGanados pista jinetes jinete, tiempoTotal pista (chocoboJinete jinete))


--7) 
fuePareja :: Pista -> [Jinete] -> Bool
fuePareja pista = puestosParejos pista . puestosJinetesMenorAMayor pista

puestosParejos :: Pista -> [Jinete] -> Bool
puestosParejos _ [] = True
puestosParejos pista (x:y:ys) = tiempoTotal pista (chocoboJinete x) < 0.1 * tiempoTotal pista (chocoboJinete y) && puestosParejos pista ys

--8)
losChocobos :: [Chocobo]
losChocobos = [rojo, amarillo, blanco, negro]

plateado :: Chocobo
plateado = (fuerza `mayorEntre` losChocobos, peso `menorEntre` losChocobos, velocidad `mayorEntre` losChocobos)

mayorEntre :: (Chocobo -> Number) -> [Chocobo] -> Number
mayorEntre criterio = maximum.entre criterio 

--Creo que cuenta como logica repetida de mayorEntre pero NT
menorEntre :: (Chocobo -> Number) -> [Chocobo] -> Number
menorEntre criterio = minimum.entre criterio

entre :: (Chocobo -> Number) -> [Chocobo] -> [Number]
entre = map


--9)
funcionHeavy :: (Ord a, Eq c) => [(a, b)] -> (c, a) -> ((a, b) -> c) -> [c]
funcionHeavy x y z
    | (fst . head) x < snd y = map z x
    | otherwise = filter (fst y ==) (map z x)