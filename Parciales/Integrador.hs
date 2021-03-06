-- 1er Integrador Estructuras de Datos.

--1
-- Propósito: dados un elemento e, un nivel n, cambia a todos los
-- elementos del nivel n por el elemento e.
-- Precondición: no tiene.
cambiarLosDelNivelNPor :: a -> Int -> Tree a -> Tree a
cambiarLosDelNivelNPor a n EmptyT = EmptyT
cambiarLosDelNivelNPor a 0 (Tree x t1 t2) = Tree a t1 t2
cambiarLosDelNivelNPor a n (Tree x t1 t2) = 
    Tree x (cambiarLosDelNivelNPor a (n-1) t1) (cambiarLosDelNivelNPor a (n-1) t2) 

--2
-- Propósito: indica si un elemento dado pertenece a alguna 
-- de las priority queue de la lista.
-- O(Log N)
perteneceAAlgunaPQ :: Ord a => a -> [PriorityQueue a] -> Bool
perteneceAAlgunaPQ a []       = False
perteneceAAlgunaPQ a (pq:pqs) = perteneceAPQ a pq || perteneceAAlgunaPQ a pqs

perteneceAPQ :: Ord a => a -> PriorityQueue a -> Bool
perteneceAPQ a emptyPQ = False
perteneceAPQ a pq      = ((maxPQ pq) == a) || (perteneceAPQ a (deleteMaxPQ pq))

--3
-- Propósito: devuelve la cantidad de hechizos que sabe un mago.
-- hechizos :: Mago -> Int

-- Propósito: devuelve el nombre de un mago.
-- nombre :: Mago -> String

-- Propósito: incrementa en uno la cantidad de hechizos que el mago sabe. 
-- aprender :: Mago -> Mago

data EscuelaDeMagia = EDM (Map Nombre Mago) (PrioriotyQueue Mago)
-- Invariantes:
-- Si un mago se encuentra en el map tambien se encuentra en la PQ.
-- No hay magos repetidos ni en el map ni en la PQ.
-- Los magos se ordenan de mayor a menor en base a la cantidad de hechizos que saben.
-- El map relaciona los nombres de los magos con el dicho mago.

-- Propósito: dada una lista de nombres de magos, incrementa en uno su cantidad de hechizos aprendidos.
-- Precondición: los magos existen en la escuela.
-- O(Log N)
enseñarA :: [String] -> EscuelaDeMagia -> EscuelaDeMagia
enseñarA            [] edm = edm
enseñarA (n:ns) (EDM m pq) = 
    enseñarA ns (EDM (enseniarM n m)
                     (enseniarPQ n pq))

-- O(Log N)
enseniarM :: Nombre -> Map Nombre Mago -> Map Nombre Mago
enseniarM n m = assocM n (aprender (fromJust(lookupM n m))) m

-- O(Log N)
enseniarPQ :: Nombre -> PriorityQueue Mago -> PriorityQueue Mago
enseniarPQ n pq = 
    if nombre (maxPQ pq) == n 
        then insertPQ (aprender (maxPQ pq)) 
                      (deleteMaxPQ pq)
        else insertPQ (maxPQ pq) 
                      (enseniarPQ n (deleteMaxPQ pq))


