-- Resolucion Magos
-- PriorityQueue = Heap a

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)
type Nombre = String
type Hechizo = String

-------------------------------------------------------------------
-- Invariantes de representacion
-- * Los magos solo conocen los hechizos del set y viceversa.
-- * Si un mago se encuentra en el map tambien se encuentra en la heap.
-- * Las claves del map se relacionan con el nombre del mago.
-------------------------------------------------------------------

-- Propósito: Devuelve una escuela vacía.
-- Eficiencia: O(1)
fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM emptyS emptyM emptyPQ

-- Propósito: Indica si la escuela está vacía.
-- Eficiencia: O(1)
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM s m pq) = isEmptyPQ pq

-- Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
-- Eficiencia: O(log M)
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar n (EDM s m pq) = 
    EDM s 
        (assocM n (crearM n) m)
        (insertPQ (crearM n) pq)

-- Propósito: Devuelve los nombres de los magos registrados en la escuela.
-- Eficiencia: O(M)
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM s m pq) = domM m 

-- Propósito: Devuelve los hechizos que conoce un mago dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe n (EDM s m pq) = 
    hechizos (fromJust (lookupM n m))

fromJust (Just x) = x

-- Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender n (EDM s m pq) =
    sizeS s - sizeS (hechizosM n m)

hechizosM n m = hechizos (fromJust (lookupM n m))

-- Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
-- Precondición: Hay al menos un mago.
-- Eficiencia: O(log M)
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM s m pq) = 
    (maxPQ pq , EDM s 
                (quitarM (maxPQ pq) m)
                (deleteMaxPQ pq)
    )

-- Eficiencia: O(log M)
quitarM :: Mago -> Map Nombre Mago -> Map Nombre Mago
quitarM mag m = deleteM (nombre m) m

-- Propósito: Enseña un hechizo a un mago existente, y 
-- si el hechizo no existe en la escuela es incorporado a la misma.
-- Nota: No importa si el mago ya conoce el hechizo dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(M log M + log H)
enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
enseñar h n (EDM s m pq) = 
    EDM (addS h s)
        (enseñarM n h m)
        (enseñarPQ n h pq)

enseñarM :: Nombre -> Hechizo -> Map Nombre Mago -> Map Nombre Mago
enseñarM n h m =
    assocM n ((aprender h fromJust(lookupM n m))) m

enseñarH :: Nombre -> Hechizo -> PriorityQueue Mago -> PriorityQueue Mago
enseñarH n h pq =
    if nombre (maxPQ pq) == n 
        then insertPQ (aprender h (maxPQ pq)) 
                      (deleteMaxPQ pq)
        else insertPQ (maxPQ pq) 
                      (enseñarH n h (deleteMaxPQ pq))

----------------------------------------------------------------
-- Usuario

-- Propósito: Retorna todos los hechizos aprendidos por los magos.
-- Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos edm =
    hechizosAprendidos' (magos edm) edm

hechizosAprendidos' :: [Magos] -> EscuelaDeMagia -> Set Hechizo
hechizosAprendidos' []     edm = emptyS 
hechizosAprendidos' (k:ks) edm = 
    unionS (hechizosDe k edm) (hechizosAprendidos' ks edm)

-- Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
-- Eficiencia: O(log M)
hayUnExperto :: EscuelaDeMagia -> Bool
hayUnExperto edm = 
    not (estaVacia edm) &&
	leFaltanAprender
	  (nombre (fst (egresarUno edm))) edm == 0

-- Propósito: Devuelve un par con la lista de magos que saben 
-- todos los hechizos dados por la escuela y la escuela sin dichos
-- magos.
-- Eficiencia: O(M log M)
egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
egresarExpertos edm = 
    if not hayUnExperto edm
        then ([], edm)
        else agregarAlumno (expertoEDM edm) (egresarExpertos (sinExperto edm))

expertoEDM edm = fst (egresarUno edm)
sinExperto edm = snd (egresarUno edm)

agregarAlumno m (ms, edm) = (m:ms, edm) 

----------------------------------------------------------------

data Mago = ConsM Nombre (Set Hechizo)

-- crearM :: Nombre -> Mago O(1)
-- nombre :: Mago -> Nombre O(1)
-- aprender :: Hechizo -> Mago -> Mago O(log H)
-- hechizos :: Mago -> Set Hechizo O(1)

-- emptyS :: Set a O(1)
-- addS :: Ord a => a -> Set a -> Set a O(log N)
-- belongsS :: Ord a => a -> Set a -> Bool O(log N)
-- unionS :: Ord a => Set a -> Set a -> Set a O(N log N)
-- sizeS :: Set a -> Int O(1)

-- emptyPQ :: PriorityQueue a O(1)
-- isEmptyPQ :: PriorityQueue a -> Bool O(1)
-- insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a O(log M)
-- maxPQ :: PriorityQueue a -> a O(1)
-- deleteMaxPQ :: Ord a => PriorityQueue a -> PriorityQueue a O(log M)

-- emptyM :: Map k v O(1)
-- assocM :: Ord k => k -> v -> Map k v -> Map k v O(log K)
-- lookupM :: Ord k => k -> Map k v -> Maybe v O(log K)
-- deleteM :: Ord k => k -> Map k v -> Map k v O(log K)
-- domM :: Map k v -> [k] O(K)
