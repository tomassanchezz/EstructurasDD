data RAList a = MkR Int (Map Int a) (Heap a)

-- a)
--------------------------------------------
-- Invariantes de representación:
-- * El Int es >= 0.
-- * En la heap están todos los valores del map.
-- * Las claves son numeros consecutivos, entre 0 y el Int.
--------------------------------------------

-- b)

-- Costo: constante, O(1)
emptyRAL :: RAList a
emptyRAL = MkR 0 emptyM emptyH

-- Costo: constante, O(1)
isEmptyRAL :: RAList a -> Bool
isEmptyRAL (MkR i m h) = i == 0

-- Costo: constante, O(1)
lengthRAL :: RAList a -> Int
lengthRAL (MkR i m h) = i

-- Costo: logaritmico, O(log n)
-- Prec.: n es un indice valido
get :: Int -> RAList a -> a
get n (MkR i m h) = fromJust (lookupM n m)

fromJust (Just x) = x

-- alternativa:
-- case lookupM n m of
	-- 	Just x -> x

-- Costo: constante, O(1)
minRAL :: Ord a => RAList a -> a
minRAL (MkR i m h) = findMin h

-- Costo: logaritmico, O(log n)
add :: Ord a => a -> RAList a -> RAList a
add x (MkR i m h) =
	MkR (i+1) (assocM i x m) (insertH x h)


-- Costo: O(n . log n)
elems :: Ord a => RAList a -> [a]
elems (MkR i m h) = 
	-- opcion 1
	extraerM (keys m) m

	-- opcion 2, no respeta el orden de los elementos
	-- porque los devuelve de menor a mayor
	-- recolectarH h


-- Costo: O(n . log n)
extraerM :: [Int] -> Map Int a -> [a]
extraerM []     m = []
extraerM (k:ks) m = 
	fromJust (lookupM k m) : (extraerM ks m)

-- Costo: O(n . log n)
-- recolectarH :: Ord a => Heap a -> [a]
-- recolectarH h =
-- 	if isEmptyH h
-- 		then []
-- 		else findMin h : recolectarH (deleteMin h)

-- Prec.: la ralist no está vacía
-- Costo: O(n . log n)
remove :: Ord a => RAList a -> RAList a
remove (MkR i m h) =
	MkR 
	  (i - 1) 
	  (deleteM (i - 1) m)
	  (deleteH (fromJust (lookupM (i - 1) m)) h)

-- Prec.:
-- * la heap no está vacía
-- * el elemento existe en la heap
deleteH :: Ord a => a -> Heap a -> Heap a
deleteH x h =
	if x == findMin h
	   then deleteMin h
	   else insertH (findMin h) (deleteH x (deleteMin h))

------------------------------------------------------

-- Prec.: el indice debe existir
set :: Ord a => Int -> a -> RAList a -> RAList a
set n x (MkR i m h) =
	MkR 
		i 
		(assocM n x m)
		insertH x (deleteH (fromJust (lookupM n m)) h)

addAt :: Ord a => Int -> a -> RAList a -> RAList a
addAt n x (MkR i m h) =
	-- 
	MkR (i+1) (agregarDesplazando n i x m) (insertH x h)

	-- Opcion 2
	MkR (i+1)
		(asociar (agregarEn n x (valores (keys m) m)) (insertH x h)

	-- Opcion 3
	MkR (i+1) (assocM n x (desplazar n i m)) (insertH x h)

-- Ejemplo:
-- 0        1       2           3         4
-- ["hola", "casa", "tomacito", "cindor", "naranju"]

-- addAt 2 "frutilla"
-- 0        1       2           3         4
-- ["hola", "casa", "tomacito", "cindor", "naranju"]

-- =>

-- Opcion 1
agregarDesplazando n i x m =
	if n == i
	   then assocM n x m
	   else agregarDesplazando n (i - 1) x (agregarAnterior i m)

agregarAnterior i m =
	assocM i (fromJust (lookupM (i - 1) m) m

-- 0        1       2           3         4          5
-- ["hola", "casa", "frutilla", tomacito", "cindor", "naranju"]

-- obtiene los valores juntandolos con su clave
valores :: [Int] -> Map Int a -> [(Int, a)] 
valores [] m = []
valores (i:is) m =
	(i, fromJust (lookupM i m)) : valores is m

agregarEn :: Int -> a -> [(Int, a)] -> [(Int, a)]
agregarEn n x [] = []
agregarEn n x (p:ps) =
	if n == fst p
	   then (n, x) : agregarEn n x ps
	   else if n < fst p
	   	       then p : agregarEn n x ps
	   	       else (fst p + 1, snd p) : agregarEn n x ps

asociar :: [(Int, a)] -> Map Int a
asociar [] = emptyM
asociar (p:ps) =
	assocM (fst p) (snd p) (asociar ps)