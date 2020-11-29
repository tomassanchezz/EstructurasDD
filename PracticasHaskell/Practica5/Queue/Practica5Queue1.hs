-- Practica 5 Queue 1
module Practica5Queue1 (
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

-- Costo = Lineal
queue :: a -> Queue a -> Queue a
queue x (Queue xs) = Queue (agregarAlFinal x xs)

-- Costo = Lineal
agregarAlFinal :: a -> [a] -> [a]
agregarAlFinal a [] = [a]
agregarAlFinal a xs = xs ++ [a] 

-- Costo = Constante
firstQ :: Queue a -> a
firstQ (Queue []) = error "La Queue debe tener al menos un elemento"
firstQ (Queue xs) = head xs 

-- Costo = Constante
dequeue :: Queue a -> Queue a
dequeue (Queue []) = error "La Queue debe tener al menos un elemento"
dequeue (Queue xs) = Queue (tail xs)




