module Practica6Map3 (
	Map,
	emptyM,
	assocM,
	lookupM,
	deleteM,
	keys
) where

data Map k v = M [k] [v] deriving Show
-- Invariante de representacion:
-- 1. La claves no estan repetidas
-- 2. Las listas tienen igual longitud

-- O(1)
emptyM :: Map k v
emptyM = Map [] []

-- O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M ks vs) = armarM (actualizar k v ks vs)

-- O(1)
armarM :: Eq k => ([k], [v]) -> Map k v
armarM kvs = M (fst kvs) (snd kvs)

-- O(n)
actualizar :: Eq k => k -> v -> [k] -> [v] -> ([k], [v])
actualizar k v [] [] = ([k], [v])
actualizar k v (k2:ks) (v2:vs) =
	if k == k2 
		then ((k:ks), (v:vs))
		else agregarUnPar k2 v2 (actualizar k v ks vs)

-- O(1)
agregarUnPar :: Eq k -> k -> v -> ([k], [v])
agregarUnPar k v (ks, vs) = ((k:ks), (v:vs)) 

-- O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M ks vs) = buscar k ks vs

-- O(n)
buscar :: Eq k => k -> [k] -> [v] -> Maybe v
buscar k [] []  = Nothing
buscar k (x:xs) (v:vs) = 
	 if k == x
	 	then Just v
	 	else buscar k xs vs

-- O(n)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M ks vs) = 
	let (ks', vs') = deleteM' k ks vs
		in M ks' vs'

deleteM' :: Eq k => k -> [k] -> [v] -> ([k], [v])
deleteM' s [] [] = ([], [])
deleteM' s (k:ks) (v:vs) = 
	if s == k
		then (ks, vs)
		else deleteMergeM k v (deleteM' s ks vs)

deleteMergeM :: k -> v -> ([k], [v]) -> ([k], [v])
deleteMergeM k v (ks, vs) = (k:ks, v:vs)

-- O(1)
keys :: Eq k => Map k v -> [k]
keys (M ks vs) = ks 





























