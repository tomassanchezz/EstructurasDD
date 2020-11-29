module Set2 (
	Set,
	emptyS,
	addS,
	belongs,
	sizeS,
	removeS,
	unionS,
	setToList
 ) where

data Set a = ConsS [a]
-- Invariantes de Representacion
-- No hay

-- Costo: constante
emptyS :: Set a
emptyS = ConsS []

-- Costo: constante
addS :: Eq a => a -> Set a -> Set a
addS x (ConsS xs) = ConsS (x:xs)

-- Costo: lineal
-- elem

-- Costo: lineal
belongs :: Eq a => a -> Set a -> Bool
belongs x (ConsS xs) = elem x xs

-- Costo: ?
sizeS :: Set a -> Int
sizeS (ConsS xs) = contarSinRepetidos xs

-- Costo: ?
contarSinRepetidos xs = undefined

-- Costo: ?
removeS :: Eq a => a -> Set a -> Set a
removeS x (ConsS xs) = ConsS (sacarAparicionesDe x xs)

-- Costo: ?
sacarAparicionesDe :: Eq a => a -> [a] -> [a]
sacarAparicionesDe = undefined

-- Costo: lineal
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs) (ConsS ys) = ConsS (append xs ys)

-- Costo: lineal
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- Sin repetidos
-- Costo: ?
setToList :: Eq a => Set a -> [a]
setToList (ConsS xs) = sinRepetidos xs

-- Costo: ?
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos = undefined