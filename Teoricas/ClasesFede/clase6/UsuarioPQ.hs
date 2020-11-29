import PriorityQueue1

-- Costo: lineal, O(n)
encolar :: Ord a => [a] -> PriorityQueue a
encolar [] = emptyPQ
encolar (x:xs) = insertPQ x (encolar xs)

-- Interfaz
-- emptyPQ :: PriorityQueue a
-- isEmptyPQ :: PriorityQueue a -> Bool
-- insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
-- findMinPQ :: Ord a => PriorityQueue a -> a
-- deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a

-- Costo: cuadratico, O(n^2)
pqToList :: Ord a => PriorityQueue a -> [a]
pqToList pq =
	if isEmptyPQ pq
	   then []
	   else findMinPQ pq : pqToList (deleteMinPQ pq)

-- Costo: cuadratico, O(n^2)
ordenar :: Ord a => [a] -> [a]
ordenar xs = pqToList (encolar xs)

-- Easter Egg:
-- Ezequiel Quaglia
-- Fondo de Doom

data Persona = P String Int

instance Show Persona where
	show (P n e) = "P {" ++ " nombre: " ++ n ++ ", edad: " ++ show e ++ " }"

instance Eq Persona where
	(P n1 e1) == (P n2 e2) = e1 == e2

instance Ord Persona where
	(P n1 e1) <= (P n2 e2) = e1 <= e2

atender :: [Persona] -> [Persona]
atender xs = ordenar xs

personas = [
     P "Jorge" 19,
     P "Cesar" 45,
     P "Estefania" 39,
     P "Dario" 15
   ]