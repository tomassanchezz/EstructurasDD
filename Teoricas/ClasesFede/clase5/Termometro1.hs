module Termometro1 (
	Termometro,
	nuevoT,
	estaVacio,
	agregarTemperatura,
	ultimaTemperatura,
	sacarUltimaTemperatura,
	maxTemperatura
 ) where

data Termometro = ConsT [Int] deriving Show

-- Invariantes de Representacion
-- No hay

-- Devuelve un termometro vacio
-- Costo: constante
nuevoT :: Termometro
nuevoT = ConsT []

-- Costo: constante
estaVacio :: Termometro -> Bool
estaVacio (ConsT ts) = null ts

-- null [] = True
-- null _  = False

-- Registra una temperatura
-- Costo: Constante
agregarTemperatura :: Int -> Termometro -> Termometro
agregarTemperatura t (ConsT ts) = ConsT (t : ts)

-- Costo: Constante
sacarUltimaTemperatura :: Termometro -> Termometro
sacarUltimaTemperatura (ConsT ts) = ConsT (tail ts)

-- Devuelve la ultima temperatura
-- Prec.: hay temperaturas registradas
-- Costo: Constante
ultimaTemperatura :: Termometro -> Int
ultimaTemperatura (ConsT ts) = head ts

-- Devuelve la maxima temperatura registrada
-- Prec.: hay temperaturas registradas
-- Costo: lineal
maxTemperatura :: Termometro -> Int
maxTemperatura (ConsT ts) = maximum ts

-- Costo: lineal
-- maximum [x] = x
-- maximum (x:xs) = max x (maximum xs)

-- Costo: constante
-- max :: Int -> Int -> Int