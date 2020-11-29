module PriorityQueue3(
  	PriorityQueue,
  	emptyPQ,
  	isEmptyPQ,
  	insertPQ,
  	findMinPQ,
  	deleteMinPQ
	) where

import BinaryHeap

data PriorityQueue a = PQ (Heap a) deriving Show

-- Costo: constante, O(1)
emptyPQ :: PriorityQueue a
emptyPQ = PQ emptyH

-- Costo: constante, O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ h) = isEmptyH h

-- Costo: logaritmico, O(log n)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ h) = PQ (insertH x h)

-- Costo: constante, O(1)
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ h) = findMinH h

-- Costo: logaritmico, O(log n)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ h) = PQ (deleteMinH h)