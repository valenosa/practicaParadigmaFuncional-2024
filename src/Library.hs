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


aplicarModificadores :: Tiro -> Obstaculo -> Tiro
aplicarModificadores (UnTiro velocidad precision altura) obstaculo = UnTiro {velocidad= (mVelocidad.modificadores) obstaculo velocidad, precision= (mPrecision.modificadores) obstaculo precision, altura= (mAltura .modificadores) obstaculo altura}

-- noImporta :: Condicion
-- noImporta _ = True

-- tiroSuperaElObstaculo :: Tiro -> Obstaculo -> Bool
-- tiroSuperaElObstaculo tiro obstaculo = condicionVelocidad obstaculo (velocidad tiro) && condicionPrecision obstaculo (precision tiro) && condicionAltura obstaculo (altura tiro)

-- tiroVsObstaculo :: Tiro -> Obstaculo -> Tiro
-- tiroVsObstaculo tiro obstaculo
--     |tiroSuperaElObstaculo tiro obstaculo = 



{- type Obstaculo = Tiro -> Tiro

tunelConRampita :: Obstaculo
tunelConRampita (UnTiro velocidad precision altura)
    |precision > 90 && altura == 0 = UnTiro{velocidad= 2 * velocidad, precision= 100, altura= 0}
    |otherwise = UnTiro{velocidad= 0, precision= 0, altura= 0}

laguna :: Number -> Obstaculo
laguna largo (UnTiro velocidad precision altura)
    |velocidad > 80 && between 1 5 altura = UnTiro{velocidad= velocidad, precision= precision, altura= altura / largo}
    |otherwise = UnTiro{velocidad= 0, precision= 0, altura= 0}


--Está mal la consigna? Dice que al no superar cualquier obstáculo, queda todo en cero, y al superar el hoyo, tambien queda todo en cero
hoyo :: Obstaculo
hoyo _ = UnTiro 0 0 0
{- hoyo (UnTiro velocidad precision altura)
    |between 5 20 velocidad && precision > 95 && altura == 0 = UnTiro{velocidad= 1, precision= 1, altura= 1}
    |otherwise = UnTiro{velocidad= 0, precision= 0, altura= 0} -} -}

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
cuantosObstaculos :: Tiro -> [Obstaculo]-> Number
cuantosObstaculos tiro = length.takeWhile (tiroSuperaElObstaculo tiro)

--c)
paloMasUtil :: Jugador -> [Obstaculo] -> String
paloMasUtil jugador = nombrePalo.paloMasUtilFuncion jugador

paloMasUtilFuncion :: Jugador -> [Obstaculo] -> Palo
paloMasUtilFuncion jugador obstaculos = maximoSegun (flip cuantosObstaculos obstaculos.golpe jugador) palos

-- cuantosObstaculos (golpe jugador) obstaculos

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