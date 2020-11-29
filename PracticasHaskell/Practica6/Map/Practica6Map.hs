module Practica6Map (
	Map,
	emptyM,
	assocM,
	lookupM,
	deleteM,
	keys
) where

data Map k v = M [(k, v)] deriving Show
-- Invariantes:
-- 1. Las k no se pueden repetir. 

-- O(1)
emptyM :: Map k v
emptyM = Map []

-- O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M []) = M [(k, v)]
assocM k v (M xs) = M (actualizar k v xs)

-- O(n)
actualizar :: k -> v -> [(k, v)]
actualizar k v [] 		= [(k, v)]
actualizar k v (x:xs) = 
	if k == fst x
		then (k, v) : xs
		else x : actualizar k v xs

-- O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M xs) = buscar k xs

-- O(n)
buscar :: Eq k => k -> [(k, v)] -> Maybe v
buscar k [] 		= Nothing
buscar k (x:xs) = 
	 if k == fst x
	 	then Just snd x
	 	else buscar k xs

-- O(n)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M xs) = M (sacarApariciones k xs)

-- O(n)
sacarApariciones :: Eq k => k -> [(k, v)] -> [(k, v)]
sacarApariciones k [] = []
sacarApariciones k (x:xs) = 
	if k == fst x
		then xs
		else x : sacarApariciones k xs 

-- O(n)
keys :: Eq k => Map k v -> [k]
keys (M xs) = claves xs

-- O(n)
claves :: Eq k => [(k, v)] -> [k]
claves [] = []
claves (x:xs) = fst x : claves xs





























