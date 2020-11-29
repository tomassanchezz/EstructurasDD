module Practica6Map3 (
	Map,
	emptyM,
	assocM,
	lookupM,
	deleteM,
	keys
) where

data Map k v = M Tree (k,v)  deriving Show

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

-- Invariante de representacion:
-- 1. Cumple con BST
-- 2. Cumple con AVL

-- Se supone que existe: 
-- armarBalanceado :: Ord a => a -> Tree a -> Tree a -> Tree a

lookupBST :: Ord k => k -> Tree (k, v) -> Maybe v
lookupBST k EmptyT = Nothing
lookupBST k (NodeT p ti td) =
	if k == fst p
	   then Just (snd p)
	   else if k < fst p
	           then lookupBST k ti
	           else lookupBST k td

emptyM :: Map k v
emptyM = EmptyT

assocM :: Eq k => k -> v -> Map k v -> Map k v


lookupM :: Eq k => k -> Map k v -> Maybe v
deleteM :: Eq k => k -> Map k v -> Map k v
keys :: Eq k => Map k v -> [k]