module Map3
  (
    Map,
    emptyM,
    assocM,
    lookupM,
    deleteM,
    keys
  ) where

data Map k v = MKV [k] [v] deriving Show
-- Invariante de representacion:
-- 1. La claves no estan repetidas
-- 2. Las listas tienen igual longitud

-- Costo: constante, O(1)
emptyM :: Map k v
emptyM = MKV [] []

-- Costo: lineal, O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (MKV ks vs) = 
  armarMKV (actualizar k v ks vs)

-- Costo: constante, O(1)
armarMKV :: ([k], [v]) -> Map k v
armarMKV pkvs = MKV (fst pkvs) (snd pkvs)

-- Costo: lineal, O(n)
actualizar :: Eq k => k -> v -> [k] -> [v] -> ([k], [v])
actualizar k v []      []      = ([k], [v])
actualizar k v (k2:ks) (v2:vs) =
  if k == k2
     then ( (k:ks) , (v:vs) )
     else agregarAlPar k2 v2 (actualizar k v ks vs)

-- Costo: constante, O(1)
agregarAlPar :: Eq k => k -> v -> ([k], [v]) -> ([k], [v])
agregarAlPar k2 v2 (ks, vs) = 
  (k2:ks, v2:vs)

-- Costo: lineal, O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (MKV ks vs) = buscar k ks vs

-- data Maybe a = Nothing | Just a

buscar k [] [] = Nothing
buscar k (k2:ks) (v:vs) =
  if k == k2
     then Just v
     else buscar k ks vs

-- Costo: lineal, O(n)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (MKV ks vs) = undefined

-- Costo: lineal, O(n)
sacarApariciones k [] = []
sacarApariciones k (pkv:kvs) =
  if k == fst pkv
     then kvs
     else pkv : sacarApariciones k kvs

-- Costo: constante, O(1)
keys :: Eq k => Map k v -> [k]
keys (MKV ks vs) = ks