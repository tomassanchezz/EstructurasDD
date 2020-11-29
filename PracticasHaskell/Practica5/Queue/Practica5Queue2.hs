-- Practica 5 Queue 2
module Practica5Queue2 (
	Queue,
	emptyQ,
	isEmptyQ,
	queue,
	firstQ,
	dequeue
 ) where

data Queue a = Queue [a]

-- Costo = Constante
emptyQ :: Queue a
emptyQ = Queue []

-- Costo = Constante
isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue xs) = null xs

-- Costo = Constante
queue :: a -> Queue a -> Queue a
queue x (Queue xs) = Queue (x:xs)

-- Costo = Lineal
firstQ :: Queue a -> a
firstQ (Queue []) = error "La Queue debe tener al menos un elemento"
firstQ (Queue xs) = elUltimoDeLaLista xs

-- Costo = Lineal 
elUltimoDeLaLista :: [a] -> a
elUltimoDeLaLista (x:xs) =
	if null xs
		then x
		else elUltimoDeLaLista xs 

-- Costo = Lineal
dequeue :: Queue a -> Queue a
dequeue (Queue []) = error "La Queue debe tener al menos un elemento"
dequeue (Queue xs) = Queue (laListaSinElUltimo xs)

-- Costo = Lineal
laListaSinElUltimo :: [a] -> [a]
laListaSinElUltimo [] = []
laListaSinElUltimo (x:xs) = 
	if null xs
		then xs
		else x : laListaSinElUltimo xs