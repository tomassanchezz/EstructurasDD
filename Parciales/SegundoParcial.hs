-- Segundo parcial

data Evidencia = String
data Nombre = String

-- p1 = p2 si tienen el mismo nombre.
-- p1 > p2 si tiene mas evidencia en contra que la otra.
-- no existen personas ni evidencia con el mismo nombre.

data Investigacion = ConsI (Map Nombre Persona)
                           (Map Evidencia [Nombre])
                           (PriorityQueue Persona)
                            Int

--------------------------------------------------------
-- Invariantes:
-- * Los nombres de la lista del map son nombres que
--   se encuentran en el primer map.
-- * Si una persona se encuentra en el map tambien se
--   encuentra en la pq y viceversa.
-- * Si el map de personas esta vacio la pq tambien.
-- * El int es la cantidad de claves del segundo map.
--   En una investigacion vacia es 0.
-- * Los nombres del primer map se relacionan con los
--   nombres de las personas.

-- Propósito: crea una investigación sin datos.
-- Eficiencia: O(1)
comenzarInvestigacion :: Investigacion
comenzarInvestigacion = ConsI emptyM emptyM emptyPQ 0

-- Propósito: devuelve la cantidad de eviencia ingresada.
-- Eficiencia: O(1)
cantEvidenciaIngresada :: Investigacion -> Int
cantEvidenciaIngresada (ConsI pers evs sosp cevs) = cevs

-- Propósito: devuelve la evidencia ingresada.
-- Eficiencia: O(N)
evidenciaIngresada :: Investigacion -> [Evidencia]
evidenciaIngresada (ConsI pers evs sosp cevs) = domM evs

-- Propósito: devuelve los nombres de personas ingresadas.
-- Eficiencia: O(N)
nombresIngresados :: Investigacion -> [Nombre]
nombresIngresados (ConsI pers evs sosp cevs) = domM pers

-- Propósito: indica si la investigación posee al menos una persona 
-- con 5 evidencias en su contra.
-- Eficiencia: O(1)
casoCerrado :: Investigacion -> Bool
casoCerrado (ConsI pers evs sosp cevs) = 
    cantEvidencia (maxPQ sosp) >= 5

-- Propósito: indica si esa persona tiene al menos una evidencia en su contra.
-- Nota: la persona puede no existir.
-- Eficiencia: O(log N)
esSospechoso :: Nombre -> Investigacion -> Bool
esSospechoso n (ConsI pers evs sosp cevs) =
    if esNothing (lookupM n pers)
        then False
        else cantEvidencia (fromJust (lookupM n pers)) > 0

esNothing :: Maybe -> Bool
esNothing Nothing = True
esNothing _       = False 

-- Eficiencia: O(1)
fromJust (Just x) = x

-- Propósito: devuelve a las personas con cero evidencia en su contra.
-- Eficiencia: O(N log N)
posiblesInocentes :: Investigacion -> [Persona]
posiblesInocentes inv =
    posiblesInocentes' (nombresIngresados inv) inv

-- Eficiencia: O(N log N)
posiblesInocentes' :: [Nombre] -> Investigacion -> [Persona]
posiblesInocentes' [] inv = []
posiblesInocentes' (n:ns) (ConsI pers evs sosp cevs) = 
    if esSospechoso n (ConsI pers evs sosp cevs)
        then posiblesInocentes' ns 
                                (ConsI pers evs sosp cevs)
        else fromJust(lookupM n pers) : posiblesInocentes' ns 
                                                           (ConsI pers evs sosp cevs)

-- Propósito: ingresa a personas nuevas a la investigación (mediante sus nombres),
-- sin evidencia en su contra.
-- Precondición: las personas no existen en la investigación y 
-- no hay nombres repetidos.
-- Eficiencia: O(N log N)
ingresarPersonas :: [Nombre] -> Investigacion -> Investigacion
ingresarPersonas ns (ConsI pers evs sosp cevs) =
    ConsI (ingresarPersonasM ns pers) evs (ingresarPersonasPQ ns sosp) cevs 

-- Eficiencia: O(N log N)
ingresarPersonasM :: [Nombre] -> Map Nombre Persona -> Map Nombre Persona
ingresarPersonasM [] m = m
ingresarPersonasM (n:ns) m = 
    ingresarPersonasM ns (assocM n (crearP n) m)

-- Eficiencia: O(N log N)
ingresarPersonasPQ :: [Nombre] -> PriorityQueue Persona -> PriorityQueue Persona
ingresarPersonasPQ [] pq = pq
ingresarPersonasPQ (n:ns) pq = 
    ingresarPersonasPQ ns (ingresarPersona (crearP n) pq)

-- Propósito: asocia una evidencia a una persona dada.
-- Precondición: la evidencia aún no está asociada a esa persona.
-- Nota: la persona y la evidencia existen, pero NO están asociadas.
-- Eficiencia: O(N log N)
-- No modifico el primer map ya que no me parece relevante si solo
-- esta para poder hacer la asociacion nombre -> persona.
ingresarEvidencia :: Evidencia -> Nombre -> Investigacion -> Investigacion
ingresarEvidencia ev n (ConsI pers evs sosp cevs) = 
    if lookupM ev evs == Nothing -- *
        then ConsI pers (assocM ev n evs) (aumentarEvidencia ev n sosp) (cevs+1)
        else ConsI pers (assocM ev n evs) (aumentarEvidencia ev n sosp) cevs

-- * esNothing (Nothing) == True

-- Eficiencia: O(N log N)
aumentarEvidencia :: Evidencia -> Nombre -> PriorityQueue Persona -> PriorityQueue Persona
aumentarEvidencia ev n pq = 
    if nombre (maxPQ pq) == n
        then insertPQ (agregarEvidencia ev (maxPQ pq)) (deleteMaxPQ pq)
        else insertPQ (maxPQ pq) (aumentarEvidencia ev n (deleteMaxPQ pq))

------------------------------------------------------------------------------------
-- Usuario:

-- Propósito: Comienza una investigación con una lista de nombres sin evidencia.
-- Eficiencia: O(N log N)
comenzarConPersonas :: [Nombre] -> Investigacion
comenzarConPersonas ns = ingresarPersonas ns comenzarInvestigacion

-- Eficiencia: O(N log N)
-- Propósito: Indica si las personas en la investigación son todas inocentes.
todosInocentes :: Investigacion -> Bool
todosInocentes inv = 
    length (posiblesInocentes (nombresIngresados inv) inv) 
    ==
    length (nombresIngresados inv)

-- Propósito: Indica si la evidencia en la lista es suficiente para cerrar el caso.
-- Eficiencia: O(N log N)
terminaCerrado :: [(Evidencia, Nombre)] -> Investigacion -> Bool
terminaCerrado [] inv = casoCerrado inv
terminaCerrado (ev:evs) inv =
    terminaCerrado evs (ingresarEvidencia (fst ev) (snd ev) inv)

-------------------------------------------------------------------------------------
-- Persona:
data Persona = P Nombre [Evidencia] Int
-- Donde int es el largo de la lista de evidencias.
-- Si la lista es vacia el int es 0.


