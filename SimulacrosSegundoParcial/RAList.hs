-- Resolucion RALIST

data RAList a = MKR Int (Map Int a) (Heap a)

-- Propósito: devuelve una lista vacía.
-- Eficiencia: O(1).
emptyRAL :: RAList a
emptyRAL = MKR 0 emptyM emptyH

-- Propósito: indica si la lista está vacía.
-- Eficiencia: O(1).
isEmptyRAL :: RAList a -> Bool
isEmptyRAL (MKR n _ _) = n == 0

-- Propósito: devuelve la cantidad de elementos.
-- Eficiencia: O(1).
lengthRAL :: RAList a -> Int
lengthRAL (MKR n _ _) = n

-- Propósito: devuelve el elemento en el índice dado.
-- Precondición: el índice debe existir.
-- Eficiencia: O(log N).
get :: Int -> RAList a -> a
get x (MKR n m h) = 
    fromJust (lookupM m x)

-- Eficiencia: O(1).
fromJust (Just x) = x

-- Propósito: devuelve el mínimo elemento de la lista.
-- Precondición: la lista no está vacía.
-- Eficiencia: O(1)
minRAL :: Ord a => RAList a -> a
minRAL (MKR n m h) = findMin h

-- Propósito: agrega un elemento al final de la lista.
-- Eficiencia: O(log N).
add :: Ord a => a -> RAList a -> RAList a
add a (MKR n m h) = 
    MKR (n+1) (assocM n a m) (insertH a)

-- Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
-- Eficiencia: O(N log N).
elems :: Ord a => RAList a -> [a]
elems (MKR n m h) = elems' (domM m) m

-- Eficiencia: O(N * log N).
elems' :: [Int] -> Map Int a -> [a]
elems' [] m = []
elems' (k:ks) m = 
    fromJust (lookupM m k) : (elems' ks m)

-- Propósito: elimina el último elemento de la lista.
-- Precondición: la lista no está vacía.
-- Eficiencia: O(N log N).
remove :: Ord a => RAList a -> RAList a
remove (MKR n m h) = 
    MKR (n-1) 
        (deleteM (n-1) m)  
        (deleteH (fromJust (lookupM (i - 1) m)) h)

-- Prec.:
-- * la heap no está vacía
-- * el elemento existe en la heap
deleteH :: Ord a => a -> Heap a -> Heap a
deleteH x h =
	if x == findMin h
	   then deleteMin h
	   else insertH (findMin h) (deleteH x (deleteMin h))

-- Propósito: reemplaza el elemento en la posición dada.
-- Precondición: el índice debe existir.
-- Eficiencia: O(N log N).
set :: Ord a => Int -> a -> RAList a -> RAList a
set x a (MKR n m h) = 
    MKR n 
        (assocM x a m) 
        (insertH a (deleteH (fromJust (lookupM x m) h)))

-- Propósito: agrega un elemento en la posición dada.
-- Precondición: el índice debe estar entre 0 
--               y la longitud de la lista.
-- Observación: cada elemento en una posición posterior a 
--              la dada pasa a estar en su posición siguiente.
-- Eficiencia: O(N log N).
-- Sugerencia: definir una subtarea que corra los elementos 
--             del Map en una posición a partir de una posición dada. Pasar
--             también como argumento la máxima posición posible.
addAt :: Ord a => Int -> a -> RAList a -> RAList a
addAt x a (MKR n m h) = 
     MKR (n+1) (insertarYDesplazar x n a m) (insertH a)

-- Eficiencia: O(log n).
insertarYDesplazar x n a m = 
    if x == n 
        then assocM x a m 
        else agregarDesplazando x (n - 1) a (agregarAnterior n m)

-- Eficiencia: O(log n).
agregarAnterior n m =
	assocM n (fromJust (lookupM (n - 1) m) m

-- La interfaz de Heap, siendo H la cantidad de elementos en la heap:
-- emptyH :: Heap a O(1)
-- isEmptyH :: Heap a -> Bool O(1)
-- findMin :: Heap a -> a O(1)
-- insertH :: Ord a => a -> Heap a -> Heap a O(log H)
-- deleteMin :: Ord a => Heap a -> Heap a O(log H)

-- La interfaz de Map, siendo K la cantidad de claves distintas en el map:
-- emptyM :: Map k v O(1)
-- assocM :: Ord k => k -> v -> Map k v -> Map k v O(log K)
-- lookupM :: Ord k => Map k v -> k -> Maybe v O(log K)
-- deleteM :: Ord k => k ->Map k v -> Map k v O(log K)
-- domM :: Map k v -> [k] O(K)