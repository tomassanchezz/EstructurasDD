-- Usuario de Set
import Practica5Set1 

-- emptyS :: Set a

-- addS :: Eq a => a -> Set a -> Set a

-- belongs :: Eq a => a -> Set a -> Bool

-- sizeS :: Eq a => Set a -> Int

-- removeS :: Eq a => a -> Set a -> Set a

-- unionS :: Eq a => Set a -> Set a -> Set a

-- setToList :: Eq a => Set a -> [a]

data Tree a = NodeT a (Tree a) (Tree a)
			| EmptyT

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s 	  = []
losQuePertenecen (x:xs) s = 
	if belongs x s
		then x : losQuePertenecen xs s
		else losQuePertenecen xs s

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos xs = setToList (toSet xs)

toSet :: Eq a => [a] -> Set a
toSet [] = emptyS
toSet (x:xs) = addS x (toSet xs)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT a t1 t2) = unionS (unionS a (unirTodos t1)) (unirTodos t2)