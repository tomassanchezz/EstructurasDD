module Termometro3 (
	Termometro,
	nuevoT,
	estaVacio,
	agregarTemperatura,
	ultimaTemperatura,
	sacarUltimaTemperatura,
	maxTemperatura
 ) where

data Termometro = ConsT [Int] [Int] deriving Show

-- Invariantes de Representacion
-- Sea ConsT ts ms
-- 1. El head de ms es el maximo de ts
-- 2. ts y ms tienen la misma cantidad de elementos

-- Devuelve un termometro vacio
-- Costo: Constante
nuevoT :: Termometro
nuevoT = ConsT [] []

-- Costo: Constante
estaVacio :: Termometro -> Bool
estaVacio (ConsT ts ms) = null ts

-- Registra una temperatura
-- Costo: Constante
agregarTemperatura :: Int -> Termometro -> Termometro
agregarTemperatura t (ConsT ts ms) = ConsT (t : ts) (calcMax t ms)

-- Costo: Constante
calcMax :: Int -> [Int] -> [Int]
calcMax t [] = [t]
calcMax t (m:ms) = max t m : m : ms

-- Prec.: hay temperaturas registradas
-- Costo: constante
sacarUltimaTemperatura :: Termometro -> Termometro
sacarUltimaTemperatura (ConsT ts ms) = ConsT (tail ts) (tail ms)

-- Devuelve la ultima temperatura
-- Prec.: hay temperaturas registradas
-- Costo: constante
ultimaTemperatura :: Termometro -> Int
ultimaTemperatura (ConsT ts tm) = head ts

-- Devuelve la maxima temperatura registrada
-- Prec.: hay temperaturas registradas
-- Costo: constante
maxTemperatura :: Termometro -> Int
maxTemperatura (ConsT ts ms) = head ms