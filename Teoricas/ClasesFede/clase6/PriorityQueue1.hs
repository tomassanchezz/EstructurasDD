module PriorityQueue1(
  	PriorityQueue,
  	emptyPQ,
  	isEmptyPQ,
  	insertPQ,
  	findMinPQ,
  	deleteMinPQ
	) where

data PriorityQueue a = PQ [a] deriving Show
-- Invariantes de representación: No tiene

-- Costo: constante, O(1)
emptyPQ :: PriorityQueue a
emptyPQ = PQ []

-- Costo: constante, O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

-- Costo: constante, O(1)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ xs) = PQ (x:xs)

-- Costo: lineal, O(n)
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ xs) = minimum xs

-- Costo: lineal, O(n)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = PQ (borrar (minimum xs) xs)

-- borra el que sea el minimo
borrar m [] = []
borrar m (x:xs) =
  if m == x
     then xs
     else x : borrar m xs