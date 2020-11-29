data Sanguche = Pan Relleno deriving Show

data Relleno = 
  Feta TipoDeFeta Relleno | 
  Aire
  deriving Show
  
data TipoDeFeta = 
  Queso | 
  Jamon | 
  Mortadela | 
  Salame
  deriving Show
  
  
-- Proposito: Dado un sanguche, indica si el relleno es solo de aire.
rellenoDeAire :: Sanguche -> Bool
rellenoDeAire (Pan relleno) = rellenoDeAire' relleno

rellenoDeAire' :: Relleno -> Bool
rellenoDeAire' Aire = True
rellenoDeAire' _ = False


-- Proposito: Dado un sanguche indica si solo tiene fetas de jamon.
esTortitaDeJamon :: Sanguche -> Bool
esTortitaDeJamon (Pan relleno) = 
  if rellenoDeAire' relleno
    then False
    else contieneALoSumoJamon relleno

contieneALoSumoJamon :: Relleno -> Bool
contieneALoSumoJamon Aire = True
contieneALoSumoJamon (Feta tipo resto) =
  esTipo Jamon tipo && contieneALoSumoJamon resto

esTipo :: TipoDeFeta -> TipoDeFeta -> Bool
esTipo Queso Queso = True
esTipo Jamon Jamon = True
esTipo Mortadela Mortadela = True
esTipo Salame Salame = True
esTipo _ _ = False

-- Proposito: Dados un numero n y un tipo de feta, agrega n fetas de 
-- ese tipo, al principio del relleno del sanguche dado.
mandaleNDe :: Int -> TipoDeFeta -> Sanguche -> Sanguche
mandaleNDe n t (Pan relleno) = Pan $ mandaleNDe' n t relleno

mandaleNDe' :: Int -> TipoDeFeta -> Relleno -> Relleno
mandaleNDe' 0 t relleno = relleno
mandaleNDe' n t relleno = 
  Feta t (mandaleNDe' (n-1) t relleno)

-- Proposito: Quita todo el queso del relleno al sanguche dado.
peroSinQueso :: Sanguche -> Sanguche
peroSinQueso (Pan relleno) = Pan $ peroSinQueso' relleno

peroSinQueso' :: Relleno -> Relleno
peroSinQueso' Aire = Aire
peroSinQueso' (Feta t resto) =  
  if esTipo t Queso
    then peroSinQueso' resto
    else Feta t $ peroSinQueso' resto


-- Proposito: Devuelve una lista de fetas del relleno del sanguche, 
-- junto con su cantidad de apariciones.
ordenadosPorCantidad :: Sanguche -> [(TipoDeFeta, Int)]
ordenadosPorCantidad (Pan relleno) = ordenadosPorCantidad' relleno

ordenadosPorCantidad' :: Relleno -> [(TipoDeFeta, Int)]
ordenadosPorCantidad' Aire = []
ordenadosPorCantidad' (Feta t resto) = 
  agregarUnidad t (ordenadosPorCantidad' resto)
  
agregarUnidad :: TipoDeFeta -> [(TipoDeFeta, Int)] -> [(TipoDeFeta, Int)]
agregarUnidad t [] = [(t,1)]
agregarUnidad t (x:xs) = 
  if esTipo t (fst x)
    then (t, (snd x) + 1): xs
    else x : agregarUnidad t xs



-- Las llaves son simples numeros.
type Llave = Int

-- Existen direcciones que indican caminos dentro del laberinto.
data Dir = Izq | Der deriving Show

-- Los cofres tienen una cantidad de oro,
-- pero solo accesible para aquellos que posean las llaves que lo abren.
-- Pueden existir cofres que no requieran de llaves.
data Cofre = Cofre [Llave] Int deriving Show

-- Un laberinto posee salidas, celdas y bifurcaciones.
-- En las celdas hay cofres, en las salidas no hay nada,
-- y en las bifurcaciones puedo ir hacia uno u otro lado.
data Laberinto = 
  Salida | 
  Celda Cofre | 
  Bifurcacion Laberinto Laberinto
  deriving Show
  

labSalida = Salida

labCelda10 = Celda (Cofre [10] 1000)
labCelda10y11 = Celda (Cofre [10,11] 50000)

lab1 = Bifurcacion 
  labSalida
  labCelda10 

lab2 = Bifurcacion (
  Bifurcacion
    labCelda10
    labCelda10 )
  labCelda10 

lab3 = Bifurcacion (
  Bifurcacion
    labCelda10
    labSalida )
  labSalida 

lab5 = Bifurcacion (
  Bifurcacion (
	Bifurcacion
	  labCelda10
	  labSalida ) (
    labCelda10y11 ))
  labCelda10 
  
-- Proposito: Indica cuantas salidas posee un laberinto.
cantidadDeSalidas :: Laberinto -> Int
cantidadDeSalidas Salida = 1
cantidadDeSalidas (Celda cofre) = 0
cantidadDeSalidas (Bifurcacion li ld) =
  cantidadDeSalidas li + cantidadDeSalidas ld

-- Proposito: Dado un laberinto indica que llaves se deben tener para 
-- poder abrir todos sus cofres.
-- Nota: el resultado no debe tener llaves repetidas.
queLlavesDeboTener :: Laberinto -> [Llave]
queLlavesDeboTener Salida = []
queLlavesDeboTener (Celda cofre) = llavesDeCofre cofre
queLlavesDeboTener (Bifurcacion li ld) =
  unirDistintos (queLlavesDeboTener li) (queLlavesDeboTener ld)

llavesDeCofre :: Cofre -> [Llave]
llavesDeCofre (Cofre llaves oro) = llaves

unirDistintos :: Eq a => [a] -> [a] -> [a]
unirDistintos [] ys = ys
unirDistintos (x:xs) ys = 
  if elem x ys
    then unirDistintos xs ys
    else x: unirDistintos xs ys

-- Proposito: Indica cuanto oro puede conseguirse dada una 
-- lista de llaves.
cantidadDeOroCon :: [Llave] -> Laberinto -> Int
cantidadDeOroCon llaves Salida = 0
cantidadDeOroCon llaves (Celda cofre) = 
  cantidadDeOroCon' llaves cofre
cantidadDeOroCon llaves (Bifurcacion li ld) =
  (cantidadDeOroCon llaves li) + (cantidadDeOroCon llaves ld)


cantidadDeOroCon' :: [Llave] -> Cofre -> Int
cantidadDeOroCon' llaves (Cofre requeridas oro) =
  if estanTodas llaves requeridas
    then oro
    else 0 

estanTodas :: Eq a => [a] -> [a] -> Bool
estanTodas xs [] = True
estanTodas xs (y:ys) = 
  elem y xs && estanTodas xs ys

-- Proposito: Dada una lista de direcciones indica si llevan a una 
-- posible salida del laberinto.
haySalidaPor :: [Dir] -> Laberinto -> Bool
haySalidaPor _ Salida = True
haySalidaPor _ (Celda cofre) = False
haySalidaPor [] (Bifurcacion li ld) = False
haySalidaPor (d:ds) (Bifurcacion li ld) = 
  case d of
    Izq -> haySalidaPor ds li
    Der -> haySalidaPor ds ld


-- Proposito: Indica el camino a la salida mas cercana.
-- Precondicion: Existe al menos una salida.
salidaMasCercana :: Laberinto -> [Dir]
salidaMasCercana Salida = []
salidaMasCercana (Celda cofre) = []
salidaMasCercana (Bifurcacion li ld) = 
  if haySalida li
    then if haySalida ld
      then caminoMasCorto (Izq:salidaMasCercana li) (Der:salidaMasCercana ld)
      else Izq: salidaMasCercana li
    else Der: salidaMasCercana ld

caminoMasCorto :: [Dir] -> [Dir] -> [Dir]
caminoMasCorto i d = 
  if length i < length d
    then i
    else d

haySalida :: Laberinto -> Bool
haySalida Salida = True
haySalida (Celda cofre) = False
haySalida (Bifurcacion li ld) = 
  haySalida li || haySalida ld

salidaMasCercana' :: Laberinto -> [Dir]
salidaMasCercana' lab = 
  case salidaMasCercana'' lab of
    (Just direcciones) -> direcciones
    Nothing -> error "no hay camino de salida"

salidaMasCercana'' :: Laberinto -> Maybe [Dir]
salidaMasCercana'' Salida = Just []
salidaMasCercana'' (Celda cofre) = Nothing
salidaMasCercana'' (Bifurcacion li ld) = 
  decidir (salidaMasCercana'' li) (salidaMasCercana'' ld) 

decidir :: Maybe [Dir] -> Maybe [Dir] -> Maybe [Dir]
decidir Nothing Nothing = Nothing
decidir Nothing (Just salidaPorDer) = Just (Der:salidaPorDer)
decidir (Just salidaPorIzq) Nothing = Just (Izq:salidaPorIzq)
decidir (Just salidaPorIzq) (Just salidaPorDer) =
  if length salidaPorIzq < length salidaPorDer
    then Just (Izq:salidaPorIzq)
    else Just (Der:salidaPorDer)






























