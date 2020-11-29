module BST where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

type BST a = Tree a

emptyBST :: Tree a
emptyBST = EmptyT

isEmptyBST :: Tree a -> Bool
isEmptyBST EmptyT = True
isEmptyBST t      = False

-- Binary Search Tree
-- (árboles de búsqueda binaria)

-- Propósito: indica si un elemento pertenece al BST
-- Prec.: el arbol es BST y está balanceado
-- Costo: logaritmico, O(log n)
belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST e EmptyT = False
belongsBST e (NodeT x ti td) =
	if e == x
	   then True
	   else if e < x
	   	       then belongsBST e ti
	   	       else belongsBST e td

-- Propósito: dada una clave devuelve su valor asociado
-- Prec.: el arbol es BST y está balanceado
-- Costo: logaritmico, O(log n)
lookupBST :: Ord k => k -> Tree (k, v) -> Maybe v
lookupBST k EmptyT = Nothing
lookupBST k (NodeT p ti td) =
	if k == fst p
	   then Just (snd p)
	   else if k < fst p
	           then lookupBST k ti
	           else lookupBST k td

-- Costo: logaritmico, O(log n)
minBST :: Ord a => Tree a -> a
minBST (NodeT x EmptyT td) = x
minBST (NodeT x ti td)     = minBST ti

-- Costo: logaritmico, O(log n)
maxBST :: Ord a => Tree a -> a
maxBST (NodeT x ti EmptyT) = x
maxBST (NodeT x ti td)     = maxBST td

-- Costo: logaritmico, O(log n)
deleteMinBST :: Ord a => Tree a -> Tree a
deleteMinBST (NodeT x EmptyT td) = td
deleteMinBST (NodeT x ti td)     = 
	NodeT x (deleteMinBST ti) td

-- Costo: logaritmico, O(log n)
-- Propósito: un elemento y un BST,
-- devuelve un BST con ese elemento agregado
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST e EmptyT = NodeT e EmptyT EmptyT
insertBST e (NodeT x ti td) = 
	if e == x
	   then NodeT x ti td
	   else if e < x
	   	       then balancear x (insertBST e ti) td
	   	       else balancear x ti (insertBST e td)

-- investigar luego
balancear :: Ord a => a -> Tree a -> Tree a -> Tree a
balancear x ti td = NodeT x ti td

-- Costo: logaritmico, O(log n)
-- Propósito: un elemento y un BST,
-- devuelve un BST sin ese elemento
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST e EmptyT = EmptyT
deleteBST e (NodeT x ti td) =
	if e == x
	   then juntar ti td
	   else if e < x
	   	       then balancear x (deleteBST e ti) td
	   	       else balancear x ti (deleteBST e td)

-- Costo: logaritmico, O(log n)
juntar :: Ord a => Tree a -> Tree a -> Tree a
juntar ti td = 
	NodeT (minBST td) ti (deleteMinBST td)

-- Variante: buscar el maximo de ti (y borrarlo)

