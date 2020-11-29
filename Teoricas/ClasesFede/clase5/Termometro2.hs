module Termometro2 (
	Termometro,
	nuevoT,
	estaVacio,
	agregarTemperatura,
	ultimaTemperatura,
	sacarUltimaTemperatura,
	maxTemperatura
 ) where

data TalVezTemp = NoHayTemp | Temp Int deriving Show

data Termometro = ConsT [Int] TalVezTemp deriving Show

-- Invariantes de Representacion
-- Sea ConsT ts tm
-- 1. Si hay temp, entonces tm es la maxima temperatura de ts

-- Devuelve un termometro vacio
-- Costo: Constante
nuevoT :: Termometro
nuevoT = ConsT [] NoHayTemp

-- Costo: Constante
estaVacio :: Termometro -> Bool
estaVacio (ConsT ts tm) = null ts

-- Registra una temperatura
-- Costo: Constante
agregarTemperatura :: Int -> Termometro -> Termometro
agregarTemperatura t (ConsT ts tm) = ConsT (t : ts) (comp t tm)

-- Costo: Constante
comp :: Int -> TalVezTemp -> TalVezTemp
comp t NoHayTemp = Temp t
comp t (Temp t2) = Temp (max t t2)

-- Prec.: hay temperaturas registradas
-- Costo: lineal
sacarUltimaTemperatura :: Termometro -> Termometro
sacarUltimaTemperatura (ConsT ts tm) = ConsT (tail ts) (calcMax (tail ts))

-- Costo: lineal
calcMax :: [Int] -> TalVezTemp
calcMax [] = NoHayTemp
calcMax ts = Temp (maximum ts)

-- Devuelve la ultima temperatura
-- Prec.: hay temperaturas registradas
-- Costo: constante
ultimaTemperatura :: Termometro -> Int
ultimaTemperatura (ConsT ts tm) = head ts

-- Devuelve la maxima temperatura registrada
-- Prec.: hay temperaturas registradas
-- Costo: constante
maxTemperatura :: Termometro -> Int
maxTemperatura (ConsT ts tm) = extraerTemp tm

extraerTemp :: TalVezTemp -> Int
extraerTemp (Temp t) = t