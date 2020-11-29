module PriorityQueue4(
  	PriorityQueue,
  	emptyPQ,
  	isEmptyPQ,
  	insertPQ,
  	findMinPQ,
  	deleteMinPQ
	) where

import BST

data PriorityQueue a = PQ (BST a) deriving Show

-- Costo: constante, O(1)
emptyPQ :: PriorityQueue a
emptyPQ = PQ emptyBST

-- Costo: constante, O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ t) = isEmptyBST t

-- Costo: logaritmico, O(log n)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ t) = PQ (insertBST x t)

-- Costo: logaritmico, O(log n)
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ t) = minBST t

-- Costo: logaritmico, O(log n)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ t) = PQ (deleteMinBST t)