-- Practica 5 Queue 2
module Practica5Queue2 (
	Queue,
	emptyQ,
	isEmptyQ,
	queue,
	firstQ,
	dequeue
 ) where

data Queue a = Queue [a] [a]
-- Invariantes de representacion.
-- 1. Si la primera lista esta vacia la queue esta vacia

-- Costo = Constante
emptyQ :: Queue a
emptyQ = Queue [] []

-- Costo = Constante
isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue xs ys) = null xs

-- Costo = Constante
queue :: a -> Queue a -> Queue a
queue x (Queue xs ys) = Queue xs (x:ys)

-- Costo = Constante
firstQ :: Queue a -> a
firstQ (Queue [] ys) = error "La Queue debe tener al menos un elemento"
firstQ (Queue xs ys) = head xs

-- Costo = Constante
dequeue :: Queue a -> Queue a
dequeue (Queue [] ys) = error "La Queue debe tener al menos un elemento"
dequeue (Queue xs ys) = Queue (tail xs) ys 
