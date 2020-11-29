type Hechizo = String
type Nombre = String

data EscuelaDeMagia = EDM (Set Hechizo)
                          (Map Nombre Mago)
                          (PriorityQueue Mago)

-------------------------------------------------
-- Invariante de Representación
-- * Los magos solo saben los hechizos del set
-- * En el Set solo estan los hechizos que los magos saben
-- * El map contiene a los mismos magos que la priority queue
-- * Cada clave del map se relaciona con el mago con mismo nombre
-------------------------------------------------

-- Costo: constante, O(1)
fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM emptyS emptyM emptyPQ

-- Costo: constante, O(1)
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM s m pq) = isEmptyPQ pq

-- Costo: O(log n)
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar n (EDM s m pq) =
	EDM s 
		(assocM n (crearM n) m)
		(insertPQ (crearM n) pq)

-- Costo: O(n)
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM s m pq) = keys m

-- Costo: O(log n)
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe n (EDM s m pq) =
	hechizos (fromJust (lookupM n m))

fromJust (Just x) = x

hechizosM n m = hechizos (fromJust (lookupM n m))

-- O(log n)
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender n (EDM s m pq) =
	sizeS s - sizeS (hechizosM n m)

-- O(log n)
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM s m pq) =
	(
		maxPQ pq, 
		EDM s 
		    (quitarM (maxPQ pq) m)
		    (deleteMaxPQ pq)
	)

quitarM mag m = deleteM (nombre mag) m

-- battlefront 2, juan ignacio herrera

enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
enseñar hech nombr (EDM s m pq) =
	EDM (addS hech s)
	    (enseñarM hech nombr m)
	    (enseñarH hech nombr pq)

-- O(log n)
enseñarM hech nombr m =
	assocM nombr 
	       (aprender hech (fromJust (lookupM nombr m))) m

-- Prec.: el mago está
enseñarH hech nombr pq =
	if nombr == nombre (findMaxPQ pq)
	   then insertH (aprender hech (findMaxPQ pq))
	                (deleteMaxPQ pq)
	   else insertH (findMaxPQ pq) 
	                (enseñarH hech nombr (deleteMaxPQ pq))

--------------------------------------------------------------------------------

-- Como usuario de EscuelaDeMagia

hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos edm = 
	pedirHechizos (magos edm) edm

pedirHechizos :: [Nombre] -> EscuelaDeMagia -> Set Hechizo
pedirHechizos []     edm = emptyS
pedirHechizos (n:ns) edm =
	unionS (hechizosDe n edm) (pedirHechizos ns edm)

hayUnExperto :: EscuelaDeMagia -> Bool
hayUnExperto edm = 
	not (estaVacia edm) &&
	leFaltanAprender
	  (nombre (fst (egresarUno edm))) edm == 0

egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
egresarExpertos edm =
	if not (hayUnExperto edm)
	   then ([], edm)
	   else agregarExperto (elExperto edm) (egresarExpertos (sinExperto edm))

elExperto edm  = fst (egresarUno edm)
sinExperto edm = snd (egresarUno edm)

agregarExperto mag (mags, edm) = (mag : mags, edm)

---------------------------------------------------

data Mago = ConsM Nombre (Set Hechizo)

