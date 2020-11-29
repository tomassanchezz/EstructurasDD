-- Resolucion Nave
-- El tipo Sector es un tipo abstracto, y representa al sector de una nave, 
-- el cual contiene componentes y tripulantes asignados.

-- El tipo Tripulante es un tipo abstracto, y representa a un tripulante dentro 
-- de la nave, el cual tiene un nombre, un rango y sectores asignados.

type SectorId = String
type Nombre   = String
type Rango    = String

-- Un sector está vacío cuando no tiene tripulantes, y la nave 
-- está vacía si no tiene ningún tripulante.

-- Puede haber tripulantes sin sectores asignados.

data Componente = LanzaTorpedos 
                | Motor Int 
                | Almacen [Barril]

data Barril = Comida 
            | Oxigeno 
            | Torpedo 
            | Combustible

data Nave = N (Map SectorId Sector) 
              (Map Nombre Tripulante) 
              (MaxHeap Tripulante)

-------------------------------------------------------------
-- Invariantes:
-- *Los tripulantes que se encuentran en el map tambien se
--  encuentran en la heap y viceversa.
-- *Todos los nombres de tripulantes son únicos.
-- *SectorId identifica al sector de forma univoca.
-------------------------------------------------------------

-- Propósito: Construye una nave con sectores vacíos, en base a
-- una lista de identificadores de sectores.
-- Eficiencia: O(S)
construir :: [SectorId] -> Nave
construir []      = N emptyM emptyM emptyH
construir (s:sts) = agregarC s (construir sts)

-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
-- Eficiencia: O(log T)
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N sts tripsM tripsH) = 
    N sts  
      (assocM n (crearT n r) tripsM) 
      (insertH (crearT n r) tripsH) 

-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
-- Eficiencia: O(log M)
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N sts tripsM tripsH) =
    sectoresT (fromJust(lookupM n tripsM))

-- Eficiencia: O(1)
fromJust (Just x) = x 

-- Propósito: Dado un sector, devuelve los tripulantes y 
-- los componentes asignados a ese sector.
-- Precondición: Existe un sector con dicho id.
-- Eficiencia: O(log S)
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector sid (N sts tripsM tripsH) = 
    (
        tripulantesS (fromJust (lookupM sid sts)),
        componentesS (fromJust (lookupM sid sts)) 
    )

-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
-- Eficiencia: O(log T)
tripulantesN :: Nave -> [Tripulante]
tripulantesN (N sts tripsM tripsH) =
    if isEmptyH tripsH
        then []
        else maxH tripsH : tripulantesN (deleteMaxH tripsH)

-- Propósito: Asigna una lista de componentes a un sector de la nave.
-- Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sid (N sts tripsM tripsH) =
    N (agregarASector' cs sid sts) tripsM tripsH

-- Eficiencia: O(C + log S)
agregarASector' :: [Componente] -> SectorId -> Map SectorId Sector -> Map SectorId Sector
agregarASector' [] sid m     = m
agregarASector' (c:cs) sid m =
    agregarASector' cs 
                    sid 
                    (assocM sid (agregarC c (fromJust(lookupM sts m))))

-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen.
-- Eficiencia: O(log S + log T + T log T)
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n sid (N sts tripsM tripsH) =
    N assocM sid (agregarT n (fromJust(lookupM sid sts))) sts
      assocM n (asignarS sid (fromJust(lookupM n tripsM))) tripsM
      tripsH

--------------------------------------------------------------------------------
-- Usuario

-- Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados)
sectores :: Nave -> Set SectorId
sectores n = 
    sectoresNoVacios (tripulantesN n) n

sectoresNoVacios :: [Tripulante] -> Nave -> Set SectorId
sectoresNoVacios [] n     = emptyS
sectoresNoVacios (t:ts) n = 
    if sizeS (sectoresAsignados (nombre t) n) > 0
        then unionS (sectoresAsignados (nombre t) n) (sectoresNoVacios ts n)
        else sectoresNoVacios ts n

-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
sinSectoresAsignados :: Nave -> [Tripulante]
sinSectoresAsignados n =
    tripulantesSinSectoresAsignados (tripulantesN n) n

tripulantesSinSectoresAsignados :: [Tripulante] -> Nave -> [Tripulante]
tripulantesSinSectoresAsignados [] n   = []
tripulantesSinSectoresAsignados (t:ts) n = 
    if sizeS (sectoresAsignados (nombre t) n) == 0
        then t : tripulantesSinSectoresAsignados ts n
        else tripulantesSinSectoresAsignados ts n 

-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
-- Eficiencia: O(s * s * log s)
barriles :: Nave -> [Barril]
barriles n = 
    barrilesDeComp (componentes (setToList (sectores n)))

-- Eficiencia: O(s * log s)
componentes :: [SectorId] -> Nave -> [Componente] 
componentes (s:sts) n = 
    snd (datosDeSector s) : componentes sts n

-- Eficiencia: O(c)
barrilesDeComp :: [Componente] -> [Barril]
barrilesDeComp []     = []
barrilesDeComp (c:cs) = 
    if esAlmacen c
        then datoAlmacen c : barrilesDeComp cs
        else barrilesDeComp cs

-- Eficiencia: O(1)
esAlmacen (Almacen x) = True
esAlmacen _           = False

-- Eficiencia: O(1)
datoAlmacen (Almacen x) = x









      

     









