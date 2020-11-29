--Practica 5 Set
module Practica5Set1 (
	Set,
	emptyS,
	addS,
	belongs,
	sizeS,
	removeS,
	unionS,
	setToList
 ) where

data Set a = ConsS [a]
-- Invariantes de Representacion
-- Sea ConsS xs un conjunto
-- 1. xs no tiene elementos repetidos

-- Costo: constante
emptyS :: Set a
emptyS = ConsS []

-- Costo: lineal
addS :: Eq a => a -> Set a -> Set a
addS x (ConsS xs) =
	if elem x xs
	   then ConsS xs
	   else ConsS (x:xs)

-- Costo: lineal
-- elem

-- Costo: lineal
belongs :: Eq a => a -> Set a -> Bool
belongs x (ConsS xs) = elem x xs

-- Costo: lineal
sizeS :: Set a -> Int
sizeS (ConsS xs) = length xs

-- Costo: lineal
removeS :: Eq a => a -> Set a -> Set a
removeS x (ConsS xs) = ConsS (sacarUno x xs)

-- Costo: lineal
sacarUno :: Eq a => a -> [a] -> [a]
sacarUno x [] = []
sacarUno x (y:ys) =
	if x == y
	   then ys
	   else y : sacarUno x ys

-- Costo: cuadratica
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs) (ConsS ys) = ConsS (unirListas xs ys)

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
setToList (ConsS xs) = xs