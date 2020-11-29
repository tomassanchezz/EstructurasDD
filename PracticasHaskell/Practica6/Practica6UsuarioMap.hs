import Practica6Map

-- emptyM :: Map k v
-- assocM :: Eq k => k -> v -> Map k v -> Map k v
-- lookupM :: Eq k => k -> Map k v -> Maybe v
-- deleteM :: Eq k => k -> Map k v -> Map k v
-- keys :: Eq k => Map k v -> [k]

valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = maybeValores (keys m) m 

maybeValores :: Eq k => [k] -> Map k v -> [Maybe v]
maybeValores [] m = []
maybeValores (k:ks) m = lookupM k m : maybeValores ks m

valores :: Eq k => Map k v -> [v]
valores m = obtenerValores (keys m) m

obtenerValores :: Eq k => [k] -> Map k v -> [v]
obtenerValores [] m     = []
obtenerValores (k:ks) m = 
	valor (lookupM k m) : obtenerValores ks m

-- Parcial cuando es Nothing
valor :: Maybe v -> v
valor Nothing = error "no se obtener un valor"
valor (Just x) = x

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] m 	= True
todasAsociadas (k:ks) m = elem k (keys m) && todasAsociadas ks m 

listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM 
listToMap (kv:kvs) = assocM (fst kv) (snd kv) (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = mapaALista (keys m) m

mapaALista :: Eq k => [k] -> Map k v -> [(k, v)]
mapaALista [] m = []
mapaALista (k:ks) m = (k, valor (lookupM k m)) : mapaALista ks m

agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq (kv:kvs) = agruparEq' kv (agruparEq kvs)

agruparEq' :: Eq k => (k, v) -> Map k [v] -> Map k [v]
agruparEq' (k, v) m = 
	agruparEq'' k v m (lookupM k m)

agruparEq'' :: Eq k => k -> v -> Map k [v] -> Maybe v -> Map k [v]
agruparEq'' k v m Nothing = assocM k [v] m
agruparEq'' k v m (Just v') = assocM k (v:v') m

-- Terminar
-- incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
-- indexar :: [a] -> Map Int a
-- ocurrencias :: String -> Map Char Int