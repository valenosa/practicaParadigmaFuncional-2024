module Library where
import PdePreludat


-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)


bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun ::Ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord b => (a -> b) -> a -> a -> a
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


--1a) Palos

data CaracteristicasPalo = UnPalo {
  nombreDelPalo :: String,
  stats :: Tiro
}deriving(Show)

type Palo = Habilidad -> CaracteristicasPalo

putter :: Palo
putter habilidad = UnPalo {nombreDelPalo= "putter", stats= UnTiro {velocidad= 10, precision= ((* 2) . precisionJugador) habilidad, altura= 0}}

madera :: Palo
madera  habilidad = UnPalo {nombreDelPalo= "madera", stats= UnTiro {velocidad= 100, precision= ((/ 2) . precisionJugador) habilidad, altura= 5}}

hierro :: Number -> Palo
hierro n habilidad = UnPalo {nombreDelPalo= "hierro" ++ show n, stats= UnTiro {velocidad= ((* n).fuerzaJugador) habilidad, precision= ((/ n) . precisionJugador) habilidad, altura= max 0 (n-3)}}


--1b)
-- 
palos :: [Palo]
palos = [putter, madera, hierro 1, hierro 2, hierro 3, hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

--2)
golpe :: Jugador -> Palo -> Tiro
golpe persona palo = (stats.palo) (habilidad persona)


--3)

type Condicion = Number -> Bool

data Condiciones = UnasCondiciones {
    cVelocidad :: Condicion,
    cPrecision :: Condicion,
    cAltura :: Condicion
} deriving (Eq, Show)

type Modificador = Number -> Number

data Modificadores = UnosModificadores {
    mVelocidad :: Modificador,
    mPrecision :: Modificador,
    mAltura :: Modificador
} deriving (Eq, Show)

data Obstaculo = UnObstaculo {
    condiciones :: Condiciones,
    modificadores :: Modificadores
} deriving (Eq, Show)


tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo {condiciones= UnasCondiciones {cVelocidad= noImporta, cPrecision= (> 90), cAltura= (==0)}, modificadores= UnosModificadores {mVelocidad= (* 2), mPrecision= (+ 100).(* 0), mAltura= (* 0)}}

laguna :: Number -> Obstaculo
laguna largo = UnObstaculo {condiciones= UnasCondiciones {cVelocidad= (> 80), cPrecision= noImporta, cAltura= between 1 5}, modificadores= UnosModificadores {mVelocidad= (* 1), mPrecision= (* 1), mAltura= (/ largo)}}

hoyo ::  Obstaculo
hoyo = UnObstaculo {condiciones= UnasCondiciones {cVelocidad= between 5 20, cPrecision= (> 95), cAltura= (==0)}, modificadores= UnosModificadores {mVelocidad= (* 0), mPrecision= (* 0), mAltura= (* 0)}} 

noImporta :: Condicion
noImporta _ = True

tiroSuperaElObstaculo :: Tiro -> Obstaculo -> Bool
tiroSuperaElObstaculo tiro obstaculo = (cVelocidad.condiciones) obstaculo (velocidad tiro) && (cPrecision.condiciones) obstaculo (precision tiro) && (cAltura.condiciones) obstaculo (altura tiro)


tiroVsObstaculo :: Tiro -> Obstaculo -> Tiro
tiroVsObstaculo tiro obstaculo 
  |tiroSuperaElObstaculo tiro obstaculo = aplicarModificadores tiro obstaculo
  |otherwise = UnTiro{velocidad= 0, precision= 0, altura= 0}

-- En mi implementación original no usé floor porque me parece más lógico que sean numeros decimales, sin embargo los obstaculos dados están pensados para ser utilizados con una implementación que redondee para abajo, por lo que se agrega floor.
aplicarModificadores :: Tiro -> Obstaculo -> Tiro
aplicarModificadores (UnTiro velocidad precision altura) obstaculo = UnTiro {velocidad= (floor . (mVelocidad.modificadores) obstaculo) velocidad, precision= (floor . (mPrecision.modificadores) obstaculo) precision, altura= (floor . (mAltura .modificadores) obstaculo) altura}


--4)
--a)
palosUtiles :: Jugador -> Obstaculo -> [String]
palosUtiles jugador obstaculo = map nombrePalo (palosUtilesFunciones jugador obstaculo)

nombrePalo :: Palo -> String
nombrePalo palo = nombreDelPalo (palo (Habilidad 0 0))

palosUtilesFunciones :: Jugador -> Obstaculo -> [Palo]
palosUtilesFunciones jugador obstaculo = filter (jugadorSuperaElObstaculo jugador obstaculo) palos

jugadorSuperaElObstaculo :: Jugador -> Obstaculo -> Palo -> Bool
jugadorSuperaElObstaculo jugador obstaculo palo = tiroSuperaElObstaculo (golpe jugador palo) obstaculo

-- Termina el parcial

--b)
-- Versión sin takeWhile (no funciona y ahora mismo no la puedo debuggear, pero la idea está)
-- cuantosObstaculos' :: Tiro -> [Obstaculo] -> Number
-- cuantosObstaculos' tiro = length.fst.foldl obstaculoATupla ([], tiro)

-- obstaculoATupla' :: ([Obstaculo], Tiro) -> Obstaculo -> ([Obstaculo], Tiro)
-- obstaculoATupla' (obstaculosPasados, tiro) obstaculo 
--     |tiroSuperaElObstaculo tiro obstaculo = (obstaculosPasados ++ [obstaculo], tiroVsObstaculo tiro obstaculo)
--     |otherwise = (obstaculosPasados, tiroVsObstaculo tiro obstaculo)

cuantosObstaculos :: Tiro -> [Obstaculo] -> Number
cuantosObstaculos tiro = length.takeWhile id.obstaculosFueronPasados tiro

obstaculosFueronPasados :: Tiro -> [Obstaculo] -> [Bool]
obstaculosFueronPasados tiro = fst.foldl obstaculoATupla ([], tiro)

obstaculoATupla :: ([Bool], Tiro) -> Obstaculo -> ([Bool], Tiro)
obstaculoATupla (fueronPasados, tiro) obstaculo = (fueronPasados ++ [tiroSuperaElObstaculo tiro obstaculo], tiroVsObstaculo tiro obstaculo)


--c)
paloMasUtil :: Jugador -> [Obstaculo] -> String
paloMasUtil jugador = nombrePalo.paloMasUtilFuncion jugador

paloMasUtilFuncion :: Jugador -> [Obstaculo] -> Palo
paloMasUtilFuncion jugador obstaculos = maximoSegun (flip cuantosObstaculos obstaculos.golpe jugador) palos


--5) 
-- No se tienen en cuenta empates
padresQuePierden :: [(Jugador, Puntos)] -> [String]
padresQuePierden leaderboard = map padre (jugadoresQuePierden leaderboard)


jugadoresQuePierden :: [(Jugador, Puntos)] -> [Jugador]
jugadoresQuePierden leaderboard = map fst (jugadoresQuePierdenConPuntos leaderboard)

jugadoresQuePierdenConPuntos :: [(Jugador, Puntos)] -> [(Jugador, Puntos)]
jugadoresQuePierdenConPuntos leaderboard = filter (/= maximoSegun puntos leaderboard) leaderboard

puntos :: (Jugador, Puntos) -> Puntos
puntos (_, b) = b