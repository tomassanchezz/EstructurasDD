--Practica 5 Set
module Practica5Set3 (
	Set,
	emptyS,
	addS,
	belongs,
	sizeS,
	removeS,
	unionS,
	setToList
 ) where

data Set a = ConsS [a] Int
-- Invariantes de Representacion
-- Sea ConsS xs n un conjunto
-- 1. xs no tiene elementos repetidos
-- 2. n es la cantidad de elementos de xs

-- Costo: constante
emptyS :: Set a
emptyS = ConsS [] 0

-- Costo: lineal
addS :: Eq a => a -> Set a -> Set a
addS x (ConsS xs n) =
	if elem x xs
	   then ConsS xs n
	   else ConsS (x:xs) (n+1)

-- Costo: lineal
-- elem

-- Costo: lineal
belongs :: Eq a => a -> Set a -> Bool
belongs x (ConsS xs n) = elem x xs

-- Costo: Constante
sizeS :: Set a -> Int
sizeS (ConsS xs n) = n

-- Costo: lineal
removeS :: Eq a => a -> Set a -> Set a
removeS x (ConsS xs n) = ConsS (sacarUno x xs) (n-1)

-- Costo: lineal
sacarUno :: Eq a => a -> [a] -> [a]
sacarUno x [] = []
sacarUno x (y:ys) =
	if x == y
	   then ys
	   else y : sacarUno x ys

-- Costo: cuadratica
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs n) (ConsS ys m) = ConsS (unirListas xs ys) 
										 (length (unirListas xs ys))

-- Costo: cuadratica
unirListas :: Eq a => [a] -> [a] -> [a]
unirListas [] ys = ys
unirListas (x:xs) ys =
	if elem x ys
	   then unirListas xs ys
	   else x : unirListas xs ys

-- Sin repetidos
-- Costo: constante
setToList :: Eq a => Set a -> [a]
setToList (ConsS xs n) = xs