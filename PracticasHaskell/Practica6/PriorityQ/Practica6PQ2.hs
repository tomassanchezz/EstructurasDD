module Practica6PQ2 (
	PriorityQueue,
	emptyPQ,
	isEmptyPQ,
	insertPQ,
	findMinPQ,
	deleteMinPQ
) where

data PriorityQueue a = PQ [a] deriving Show
-- Invariantes:
-- 1. Todos elementos estan ordenados de menor
-- 		a mayor.

-- O(1)
emptyPQ :: PriorityQueue a
emptyPQ = PQ []

-- O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

-- O(n)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ xs) = PQ (insertarEnOrd x xs)

-- O(n)
insertarEnOrd :: Ord a -> a -> [a] -> [a]
insertarEnOrd x [] = [x]
insertarEnOrd x (y:ys) = 
	if x < y
		then x : (y:ys)
		else y : insertarEnOrd x ys

-- O(1)
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ xs) = head xs

-- O(1)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = PQ (tail xs)
