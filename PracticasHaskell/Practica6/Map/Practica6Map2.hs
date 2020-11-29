module Practica6Map2 (
	Map,
	emptyM,
	assocM,
	lookupM,
	deleteM,
	keys
) where

data Map k v = M [(k, v)] deriving Show
-- Invariantes:
-- No tiene 

-- O(1)
emptyM :: Map k v
emptyM = Map []

-- O(1)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M xs) = M (k, v) : xs

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
		then sacarApariciones k xs
		else x : sacarApariciones k xs 

-- O(n^2)
-- No admite repetidos
keys :: Eq k => Map k v -> [k]
keys (M xs) = sinRepetidos (claves xs)

-- O(n)
claves :: Eq k => [(k, v)] -> [k]
claves [] = []
claves (x:xs) = fst x : claves xs

-- O(n^2)
sinRepetidos :: Eq k => [k] -> [k]
sinRepetidos [] = []
sinRepetidos (k:ks) =
  if elem k ks
     then sinRepetidos ks
     else k : sinRepetidos ks





























