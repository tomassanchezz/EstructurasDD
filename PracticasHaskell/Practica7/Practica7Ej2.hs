-- Ejercicio 2 
-- Reciben un arbol binario que cumple los invariantes BST
-- sin elementos repetidos. Todo se debe implementar en O(log n).

module Practica7Ej2 where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

type BST a = Tree a

emptyBST :: Tree a
emptyBST = EmptyT

isEmptyBST :: Tree a -> Bool
isEmptyBST EmptyT = True
isEmptyBST t      = False

balanceado :: Tree a -> Bool
balanceado EmptyT = True
balanceado (NodeT x ti td) = abs (heightT ti - heightT td) <= 1

-- Practica 3 ------------------------------------------------------
heightT :: Tree a -> Int
heightT EmptyT                  = 0
heightT (NodeT a EmptyT EmptyT) = 0
heightT (NodeT a t1 t2)         = 1 + max (heightT t1) (heightT t2)
--------------------------------------------------------------------

insertBST :: Ord a => a -> Tree a -> Tree a
insertBST e EmptyT = NodeT e EmptyT EmptyT
insertBST e (NodeT x ti td) = 
	if e == x
	   then NodeT x ti td
	   else if e < x
	   	       then balancear x (insertBST e ti) td
	   	       else balancear x ti (insertBST e td)

-- balancear :: Ord a => a -> Tree a -> Tree a -> Tree a

deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST e EmptyT = EmptyT
deleteBST e (NodeT x ti td) = 
  if e == x
    then juntar ti td
    else if e < x 
      then balancear x (deleteBST e ti) td
      else balancear x ti (deleteBST e td) 

juntar :: Ord a => Tree a -> Tree a -> Tree a
juntar ti td = 
  NodeT (minBST td) ti (deleteMinBST td)

minBST :: Ord a => Tree a -> a
minBST (NodeT x EmptyT td) = x
minBST (NodeT x ti td)     = minBST ti

maxBST :: Ord a => Tree a -> a
maxBST (NodeT x ti EmptyT) = x
maxBST (NodeT x ti td)     = maxBST td

deleteMinBST :: Ord a => Tree a -> Tree a
deleteMinBST (NodeT x EmptyT td) = td
deleteMinBST (NodeT x ti td)     = NodeT x (deleteMinBST ti) td

deleteMaxBST :: Ord a => Tree a -> Tree a
deleteMaxBST (NodeT x ti EmptyT) = ti
deleteMaxBST (NodeT x ti td)     = NodeT x ti (deleteMaxBST td) 

perteneceBST :: Ord a => a -> Tree a -> Bool
perteneceBST e EmptyT = False
perteneceBST e (NodeT x ti td) =
	if e == x
	   then True
	   else if e < x
	   	       then belongsBST e ti
	   	       else belongsBST e td

splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST t = (minBST t, deleteMinBST t)

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST t = (maxBST t, deleteMaxBST t)


-- elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
-- elMaximoMenorA e EmptyT = Nothing
-- elMaximoMenorA e (NodeT x ti td) = 

-- elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
-- elMaximoMenorA e EmptyT = Nothing
-- elMaximoMenorA e (NodeT x ti td) = 

t1 :: Tree Int
t1 = NodeT 32
        (NodeT 35
          (NodeT 100
              (NodeT 15
                  EmptyT
                  EmptyT)
              EmptyT)
          EmptyT)
        (NodeT 40
          (NodeT 50
              EmptyT
              EmptyT)
          EmptyT)
