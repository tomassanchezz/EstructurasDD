module BinaryHeap (
    Heap,
    emptyH, -- O(1)
    isEmptyH, -- O(1)
    insertH, -- O(log n)
    findMinH, -- O(1)
    deleteMinH -- O(log n)
  )
where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Dir = Izq | Der deriving (Eq, Show)

nextPos []       = [Izq]
nextPos (Izq:ds) = Der : ds
nextPos (Der:ds) = Izq : nextPos ds

prevPos [Izq]    = []
prevPos (Der:ds) = Izq : ds
prevPos (Izq:ds) = Der : prevPos ds

data Heap a = H [Dir] (Tree a) deriving Show
--   INVARIANTES DE REPRESENTACION:
--   * El arbol es completo (todos sus niveles están completos de izquierda a derecha)
--   * En la raiz del arbol se encuentra el mínimo elemento, y lo mismo
--     sucede en cada subárbol
--   * La lista de direcciones indica el camino a un EmptyT
--     que de ser completado por una hoja, el arbol seguiría siendo completo

-- Propósito: denota una heap vacía
-- Eficiencia: O(1)
emptyH :: Heap a
emptyH = H [] EmptyT

-- Propósito: indica si la heap está vacía
-- Eficiencia: O(1)
isEmptyH :: Heap a -> Bool
isEmptyH (H _ t) = isEmptyT t

isEmptyT EmptyT = True
isEmptyT _      = False

-- Propósito: devuelve el mínimo elemento de la heap
-- Prec.: la heap no debe estar vacía
-- Eficiencia: O(1)
findMinH :: Ord a => Heap a -> a
findMinH (H _ t) = root t

root (NodeT x _ _) = x

-- Propósito: inserta un elemento en la heap
-- Eficiencia: O(log n)
insertH :: Ord a => a -> Heap a -> Heap a
insertH x (H ds t) =
  H (nextPos ds) 
    (insertAt (reverse ds) x t)

-- Propósito: inserta un elemento en la dirección apuntada,
-- devolviendo un árbol heap con el elemento agregado
-- Prec.: el árbol dado es heap
insertAt []       e EmptyT          = NodeT e EmptyT EmptyT
insertAt (Izq:ds) e (NodeT x ti td) = flotarIzq x (insertAt ds e ti) td
insertAt (Der:ds) e (NodeT x ti td) = flotarDer x ti (insertAt ds e td)

-- Propósito: chequea si debe intercambiar x con r,
-- para mantener los invariantes de heap
-- Prec.: los subarboles son heap
flotarIzq x (NodeT r tii tid) td =
  if x <= r
   then NodeT x (NodeT r tii tid) td
   else NodeT r (NodeT x tii tid) td

-- Propósito: chequea si debe intercambiar x con r,
-- para mantener los invariantes de heap
-- Prec.: los subarboles son heap
flotarDer x ti (NodeT r tdi tdd) =
  if x <= r 
   then NodeT x ti (NodeT r tdi tdd)
   else NodeT r ti (NodeT x tdi tdd)

-- Propósito: borra el mínimo elemento de la heap
-- Eficiencia: O(log n)
deleteMinH :: Ord a => Heap a -> Heap a
deleteMinH (H ds t) = 
  H (prevPos ds)
    (deleteMinT (reverse (prevPos ds)) t)

-- Prec.: el arbol dado es completo
deleteMinT ds t = replaceRoot (elemAt ds t) (deleteAt ds t)

-- Propósito:
-- borra la raiz actual (el mínimo), e inserta
-- x de tal manera que el arbol resultante sea heap
replaceRoot x (NodeT _ ti td) = hundir x ti td

-- Propósito: devuelve el elemento apuntado por el camino
-- Prec.: el arbol dado es completo
elemAt [] (NodeT x EmptyT EmptyT) = x
elemAt (Izq:ds) (NodeT x ti td) = elemAt ds ti
elemAt (Der:ds) (NodeT x ti td) = elemAt ds td

-- Propósito: borra el elemento apuntado por el camino
-- Prec.: el arbol dado es completo
deleteAt [] (NodeT x EmptyT EmptyT) = EmptyT
deleteAt (Izq:ds) (NodeT x ti td) = NodeT x (deleteAt ds ti) td
deleteAt (Der:ds) (NodeT x ti td) = NodeT x ti (deleteAt ds td)

-- Propósito: inserta x de tal forma de que el resultado sea una heap
-- Prec.: hundir toma cualquier par de árboles que sean heaps
hundir x EmptyT EmptyT = NodeT x EmptyT EmptyT

hundir x (NodeT ri tii tid) EmptyT =
  if x <= ri
     then NodeT x  (NodeT ri tii tid) EmptyT -- x es el menor del árbol
     else NodeT ri (hundir x tii tid) EmptyT -- ri es menor, hunde más a x

hundir x EmptyT (NodeT rd tdi tdd) =
  if x <= rd
     then NodeT x  EmptyT (NodeT rd tdi tdd) -- x es el menor del árbol
     else NodeT rd EmptyT (hundir x tdi tdd) -- rd es menor, hunde más a x

hundir x (NodeT ri tii tid) (NodeT rd tdi tdd) =
  if x <= ri && x <= rd -- x es el menor del árbol
     then NodeT x (NodeT ri tii tid) (NodeT rd tdi tdd) -- lo reconstruye sin hundir
     else if ri <= rd  -- x es mayor que el mas chico de ri y rd
             then NodeT ri (hundir x tii tid) (NodeT rd tdi tdd) -- hunde por la izq
             else NodeT rd (NodeT ri tii tid) (hundir x tdi tdd) -- hunde por la der