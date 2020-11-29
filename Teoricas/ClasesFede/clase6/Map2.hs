module Map2
  (
    Map,
    emptyM,
    assocM,
    lookupM,
    deleteM,
    keys
  ) where

data Map k v = MKV [(k, v)] deriving Show
-- Invariante de representacion:
-- La claves no estan repetidas

-- Costo: constante, O(1)
emptyM :: Map k v
emptyM = MKV []

-- Costo: lineal, O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (MKV kvs) = MKV (actualizar k v kvs)

actualizar k v [] = [(k, v)]
actualizar k v (pkv:kvs) =
  if k == fst pkv
     then (k,v) : kvs
     else pkv : actualizar k v kvs

-- Costo: lineal, O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (MKV kvs) = buscar k kvs

-- data Maybe a = Nothing | Just a

-- Costo: lineal, O(n)
buscar :: Eq k => k -> [(k, v)] -> Maybe v
buscar k [] = Nothing
buscar k (pkv:kvs) =
  if k == fst pkv
     then Just (snd pkv)
     else buscar k kvs

-- Costo: lineal, O(n)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (MKV kvs) = MKV (sacarApariciones k kvs)

-- Costo: lineal, O(n)
sacarApariciones k [] = []
sacarApariciones k (pkv:kvs) =
  if k == fst pkv
     then kvs
     else pkv : sacarApariciones k kvs

-- Costo: lineal, O(n)
keys :: Eq k => Map k v -> [k]
keys (MKV kvs) = soloClaves kvs

soloClaves :: Eq k => [(k, v)] -> [k]
soloClaves [] = []
soloClaves (pkv:kvs) = fst pkv : soloClaves kvs