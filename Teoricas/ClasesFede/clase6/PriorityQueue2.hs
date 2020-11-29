module PriorityQueue1(
  	PriorityQueue,
  	emptyPQ,
  	isEmptyPQ,
  	insertPQ,
  	findMinPQ,
  	deleteMinPQ
	) where

data PriorityQueue a = PQ [a] deriving Show
-- Invariantes de representación:
-- 1. la lista está ordenada de menor a mayor

-- Costo: constante, O(1)
emptyPQ :: PriorityQueue a
emptyPQ = PQ []

-- Costo: constante, O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

-- Costo: lineal, O(n)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ xs) = undefined
-- insertar de tal manera que la lista quede ordenada

-- Costo: constante, O(1)
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ xs) = head xs

-- Costo: constante, O(1)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = PQ (tail xs)

-- borra el que sea el minimo
borrar m [] = []
borrar m (x:xs) =
  if m == x
     then xs
     else x : borrar m xs