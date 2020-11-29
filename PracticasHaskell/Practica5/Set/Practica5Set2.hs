-- Practica 5 Set 2
module Practica5Set2 (
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
-- No hay

-- Costo: constante
emptyS :: Set a
emptyS = ConsS []

-- Costo: constante
addS :: Eq a => a -> Set a -> Set a
addS x (ConsS xs) = ConsS (x:xs)

-- Costo: lineal
-- elem

-- Costo: lineal
belongs :: Eq a => a -> Set a -> Bool
belongs x (ConsS xs) = elem x xs

-- Costo: Cuadratica
sizeS :: Eq a => Set a -> Int
sizeS (ConsS xs) = contarSinRepetidos xs

-- Costo: Cuadratica?
contarSinRepetidos :: Eq a => [a] -> Int
contarSinRepetidos xs = length (sinRepetidos xs)

-- Costo: Lineal
removeS :: Eq a => a -> Set a -> Set a
removeS x (ConsS xs) = ConsS (sacarAparicionesDe x xs)

-- Costo: Lineal
sacarAparicionesDe :: Eq a => a -> [a] -> [a]
sacarAparicionesDe a [] = []
sacarAparicionesDe a (x:xs) =
	if a == x
		then sacarAparicionesDe a xs
		else x : sacarAparicionesDe a xs

-- Costo: lineal
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs) (ConsS ys) = ConsS (append xs ys)

-- Costo: lineal
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- Sin repetidos
-- Costo: Lineal
setToList :: Eq a => Set a -> [a]
setToList (ConsS xs) = sinRepetidos xs

-- Costo: Cuadratica
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = 
	if elem x xs
		then xs
		else x : sinRepetidos xs 