module Practica6PQ (
	PriorityQueue,
	emptyPQ,
	isEmptyPQ,
	insertPQ,
	findMinPQ,
	deleteMinPQ
) where

data PriorityQueue a = PQ [a] deriving Show
-- Invariantes:
-- No tiene

-- O(1)
emptyPQ :: PriorityQueue a
emptyPQ = PQ []

-- O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

-- O(1)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
inserPQ x (PQ xs) = PQ (x:xs)

-- O(n)
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ xs) = minimum xs

-- O(n)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = PQ (borrar (minimum xs) xs)

-- O(n)
borrar :: Ord a => a -> [a] -> [a]
borrar x [] 	= []
borrar x (y:ys) = 
	if x == y 
		then ys
		else y : borrar x ys
