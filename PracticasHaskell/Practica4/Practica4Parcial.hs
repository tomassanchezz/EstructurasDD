-- Ejercicio 1 

data Pizza = Prepizza
           | Capa Ingrediente Pizza
           deriving Show

data Ingrediente = Salsa
                 | Queso
                 | Jamon
                 | Aceitunas Int
                 deriving Show

--Ejercicios de Practica -------------------------------------------------------------------------
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing ps) = 1 + cantidadDeCapas ps

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing ps) = 
  if esJamon ing
    then sacarJamon ps
    else Capa ing (sacarJamon ps)

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing ps) = esSalsaOQueso ing && tieneSoloSalsaYQueso ps

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing ps) = 
  if esAceituna ing
    then Capa (aceitunasDuplicadas ing) (duplicarAceitunas ps)
    else Capa ing (duplicarAceitunas ps)

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = (cantCapas x, x) : cantCapasPorPizza xs
--------------------------------------------------------------------------------------------------
cantCapas :: Pizza -> Int
cantCapas Prepizza = 0
cantCapas (Capa ing ps) = 1 +  cantCapas ps

aceitunasDuplicadas :: Ingrediente -> Ingrediente
aceitunasDuplicadas (Aceitunas x) = Aceitunas (x*2)

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Queso = True
esSalsaOQueso Salsa = True
esSalsaOQueso _     = False

ingredientes :: Pizza -> [Ingrediente]
ingredientes Prepizza = []
ingredientes (Capa ing ps) = ing : ingredientes ps

soloQueso :: Pizza -> Bool
soloQueso Prepizza = True
soloQueso (Capa ing ps) = esQueso ing && soloQueso ps

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

soloAceitunas :: Pizza -> Bool
soloAceitunas Prepizza = True
soloAceitunas (Capa ing ps) = esAceituna ing && soloAceitunas ps

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _             = False

sacarAceitunas :: Pizza -> Pizza
sacarAceitunas Prepizza = Prepizza
sacarAceitunas (Capa ing ps) = 
    if esAceituna ing
        then sacarAceitunas ps
        else Capa ing (sacarAceitunas ps)

cuantasCapasTieneJamon :: Pizza -> Int
cuantasCapasTieneJamon Prepizza = 0
cuantasCapasTieneJamon (Capa ing ps) = 
    if esJamon ing
        then 1 + cuantasCapasTieneJamon ps
        else cuantasCapasTieneJamon ps

-- Version casi difficult
-- (if es jamon ing then 1 else 0) + cuantasCap

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

cantIngNoAceituna :: Pizza -> [(Ingrediente, Int)]
cantIngNoAceituna Prepizza = []
cantIngNoAceituna (Capa ing ps) =
    sumarIng ing (cantIngNoAceituna ps)

sumarIng :: Ingrediente -> [(Ingrediente, Int)] -> [(Ingrediente, Int)]
sumarIng ing []     = [(ing, 1)]
sumarIng ing (x:xs) =
    if ingredientesIguales ing (fst x)
        then (ing, 1 + snd x) : xs
        else x : sumarIng ing xs

-- No contempla aceitunas
ingredientesIguales :: Ingrediente -> Ingrediente -> Bool
ingredientesIguales Queso Queso = True
ingredientesIguales Jamon Jamon = True
ingredientesIguales Salsa Salsa = True
ingredientesIguales _     _     = False

ejemplo1 :: Pizza
ejemplo1 = Capa Jamon (Capa Queso (Capa Salsa Prepizza))

ejemplo2 :: Pizza
ejemplo2 = Capa (Aceitunas 8) (Capa Queso Prepizza)

ejemplo3 :: Pizza
ejemplo3 = Prepizza

ejemplo4 :: Pizza
ejemplo4 = Capa Queso (Capa Jamon (Capa Queso Prepizza))
--------------------------------------------------------------------------------------------------
-- Ejercicio 2

data Dir = Izq | Der deriving Show

data Objeto = Tesoro | Chatarra deriving Show

data Cofre = Cofre [Objeto]

data Mapa = Fin Cofre
          | Bifurcacion Cofre Mapa Mapa

mapa1 :: Mapa
mapa1 = Bifurcacion (Cofre [Tesoro])
                    (Fin (Cofre [Tesoro]))
                    (Bifurcacion (Cofre [Tesoro])
                                 (Fin (Cofre [Tesoro]))
                                 (Fin (Cofre [Chatarra])))

mapa2 :: Mapa
mapa2 = 
  Bifurcacion (Cofre [Chatarra])
              (Bifurcacion (Cofre [Tesoro])
                            mapa1
                            mapa1)
              (Fin (Cofre [Tesoro]))

mapa3 :: Mapa
mapa3 = Bifurcacion (Cofre [Chatarra]) 
                    mapa2
                    mapa2

mapa4 :: Mapa
mapa4 = Bifurcacion (Cofre [Chatarra])
                    mapa2
                    mapa3

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = tieneTesoro c
hayTesoro (Bifurcacion c m1 m2) = 
  tieneTesoro c || hayTesoro m1 || hayTesoro m2

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre obs) = algunoEsTesoro obs

algunoEsTesoro :: [Objeto] -> Bool
algunoEsTesoro []     = False
algunoEsTesoro (x:xs) = esTesoro x || algunoEsTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c) = tieneTesoro c
hayTesoroEn [] (Bifurcacion c m1 m2) = tieneTesoro c
hayTesoroEn (x:xs) (Bifurcacion c m1 m2) = 
  if esDerecha x
    then hayTesoroEn xs m2
    else hayTesoroEn xs m1

esDerecha :: Dir -> Bool
esDerecha Der = True
esDerecha _   = False

caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) = 
  if tieneTesoro c
    then []
    else ladoConTesoro m1 m2 : caminoAlTesoro (mapaConTesoro m1 m2)

ladoConTesoro :: Mapa -> Mapa -> Dir
ladoConTesoro m1 m2 = 
  if hayTesoro m1 
    then Izq
    else Der

mapaConTesoro :: Mapa -> Mapa -> Mapa
mapaConTesoro m1 m2 = 
  if hayTesoro m1 
    then m1
    else m2

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) =
  if heightMap m1 > heightMap m2
    then Izq : caminoDeLaRamaMasLarga m1
    else Der : caminoDeLaRamaMasLarga m2

heightMap :: Mapa -> Int
heightMap (Fin c) = 0
heightMap (Bifurcacion c m1 m2) = 1 +  max (heightMap m1) (heightMap m2)

-- No contempla que haya mas de un tesoro por cofre
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = if tieneTesoro c then [[Tesoro]] else []
tesorosPorNivel (Bifurcacion c m1 m2) = 
  if tieneTesoro c
    then [Tesoro] : unirPerLevel (tesorosPorNivel m1) (tesorosPorNivel m2)
    else unirPerLevel (tesorosPorNivel m1) (tesorosPorNivel m2)

unirPerLevel :: [[a]] -> [[a]] -> [[a]]
unirPerLevel [] ys         = ys
unirPerLevel xs []         = xs
unirPerLevel (x:xs) (y:ys) = (x ++ y) : unirPerLevel xs ys

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c) = []
todosLosCaminos (Bifurcacion c m1 m2) = 
	if esFin m1 && esFin m2
		then [[Izq]]
		else agregarATodos Izq (todosLosCaminos m1) ++ agregarATodos Der (todosLosCaminos m2) 

agregarATodos :: a -> [[a]] -> [[a]]
agregarATodos x [] = []
agregarATodos x (y:ys) = (x : y) : (agregarATodos x ys)

esFin :: Mapa -> Bool
esFin (Fin c) = True
esFin _				= False
---------------------------------------------------------------------------------------------------
-- Ejercicio 3
data Componente = LanzaTorpedos 
                | Motor Int 
                | Almacen [Barril]

data Barril = Comida 
            | Oxigeno 
            | Torpedo 
            | Combustible

data Sector = S SectorId [Componente] [Tripulante]

type SectorId = String

type Tripulante = String

data Tree a = EmptyT 
            | NodeT a (Tree a) (Tree a)

data Nave = N (Tree Sector)

sectores :: Nave -> [SectorId]
sectores (N t) = sectoresT t

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT = []
sectoresT (NodeT s t1 t2) = idDeSector s : (sectoresT t1 ++ sectoresT t2)

idDeSector :: Sector -> SectorId
idDeSector (S id _ _) = id

poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = poderDePropulsionT t

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT EmptyT = 0
poderDePropulsionT (NodeT s t1 t2) = 
  poderDeSector s + poderDePropulsionT t1 + poderDePropulsionT t2

poderDeSector :: Sector -> Int
poderDeSector (S id comp trip) = poderDeComponentes comp

poderDeComponentes :: [Componente] -> Int
poderDeComponentes [] = 0
poderDeComponentes (x:xs) = 
  if esMotor x
    then poder x + poderDeComponentes xs
    else poderDeComponentes xs

esMotor :: Componente -> Bool
esMotor (Motor _) = True
esMotor _         = False

poder :: Componente -> Int
poder (Motor x) = x
poder _         = 0

barriles :: Nave -> [Barril]
barriles (N t) = barrilesT t

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT s t1 t2) = barrilesSector s ++ barrilesT t1 ++ barrilesT t2

barrilesSector :: Sector -> [Barril]
barrilesSector (S id comp trip) = barrilesDeComp comp

barrilesDeComp :: [Componente] -> [Barril]
barrilesDeComp [] = []
barrilesDeComp (x:xs) = 
  if esAlmacen x
    then barrilesDeAlmacen x ++ barrilesDeComp xs
    else barrilesDeComp xs

esAlmacen :: Componente -> Bool
esAlmacen (Almacen _) = True
esAlmacen _           = False

barrilesDeAlmacen :: Componente -> [Barril]
barrilesDeAlmacen (Almacen []) = []
barrilesDeAlmacen (Almacen bs) = bs

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N t) = N (agregarASectorT cs id t)

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector 
agregarASectorT cs id (EmptyT) = EmptyT
agregarASectorT cs id (NodeT s t1 t2) =
  if mismoId id s
    then NodeT (agregarCompS cs s) t1 t2
    else NodeT s (agregarASectorT cs id t1) (agregarASectorT cs id t2)

mismoId :: SectorId -> Sector -> Bool
mismoId id (S ids _ _) = id == ids

agregarCompS :: [Componente] -> Sector -> Sector
agregarCompS cs (S id comps trip) = S id (comps ++ cs) trip

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t [] n = n
asignarTripulanteA tr sids (N t) = N (asignarTripulanteAT tr sids t)

asignarTripulanteAT :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteAT tr sids EmptyT = EmptyT
asignarTripulanteAT tr (id:sids) (NodeT s t1 t2) = 
	if mismoId id s
		then NodeT (agregarTripAS tr s) (asignarTripulanteAT tr sids t1) (asignarTripulanteAT tr sids t2)
		else NodeT s (asignarTripulanteAT tr sids t1) (asignarTripulanteAT tr sids t2)

agregarTripAS :: Tripulante -> Sector -> Sector
agregarTripAS tr (S id comps trip) = S id comps (tr:trip)

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tr (N t) = sectoresAsignadosT tr t

sectoresAsignadosT :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosT tr EmptyT = []
sectoresAsignadosT tr (NodeT s t1 t2) = 
	if estaTripEnSec tr s 
		then idDeSector s : (sectoresAsignadosT tr t1 ++ sectoresAsignadosT tr t2)
		else sectoresAsignadosT tr t1 ++ sectoresAsignadosT tr t2

estaTripEnSec :: Tripulante -> Sector -> Bool
estaTripEnSec tr (S id comps trip) = estaEnTrip tr trip

estaEnTrip :: Tripulante -> [Tripulante] -> Bool
estaEnTrip tr [] = False
estaEnTrip tr (t:ts) = tr == t || estaEnTrip tr ts

tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = sinRepetidos(tripulantesT t)

tripulantesT :: Tree Sector -> [Tripulante]
tripulantesT EmptyT = []
tripulantesT (NodeT s t1 t2) = tripulantesS s ++ tripulantesT t1 ++ tripulantesT t2

tripulantesS :: Sector -> [Tripulante]
tripulantesS (S id comp trip) = trip

sinRepetidos ::Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = 
	if pertenece x xs
		then sinRepetidos xs
		else x : sinRepetidos xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = x == a || pertenece a xs
-----------------------------------------------------------------------------------------------------------------------------
type Presa = String -- nombre de presa

type Territorio = String -- nombre de territorio

type Nombre = String -- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
					| Explorador Nombre [Territorio] Lobo Lobo
					| Cria Nombre deriving Show

data Manada = M Lobo

lobo1 :: Lobo
lobo1 = Cazador "Tom" ["p1","p2","p3","p4","p5"] lobo2 lobo3 lobo4

lobo2 :: Lobo
lobo2 = Explorador "Nico" ["t1","t2"] lobo5 lobo6

lobo3 :: Lobo
lobo3 = Cazador "Julio" ["p1","p2"] lobo7 lobo8 lobo9

lobo4 :: Lobo
lobo4 = Cria "Cria1"

lobo5 :: Lobo
lobo5 = Cria "Cria2"

lobo6 :: Lobo
lobo6 = Cria "Cria3"

lobo7 :: Lobo
lobo7 = Cria "Cria4"

lobo8 :: Lobo
lobo8 = Cria "Cria5"

lobo9 :: Lobo
lobo9 = Cazador "Pitchiot" ["p1","p2","p2","p2","p2","p2","p2","p2","p2","p2"] lobo4 lobo5 lobo6

manada :: Manada
manada = M lobo1

buenaCaza :: Manada -> Bool
buenaCaza (M l) = cantDeCrias l <= cantDePresasTotales l

cantDeCrias :: Lobo -> Int
cantDeCrias (Cria n) = 1
cantDeCrias (Explorador n ts l1 l2) = cantDeCrias l1 + cantDeCrias l2
cantDeCrias (Cazador n ts l1 l2 l3) = cantDeCrias l1 + cantDeCrias l2 + cantDeCrias l3

cantDePresasTotales :: Lobo -> Int
cantDePresasTotales (Cria n) = 0
cantDePresasTotales (Explorador n ts l1 l2) = cantDePresasTotales l1 + cantDePresasTotales l2
cantDePresasTotales (Cazador n ps l1 l2 l3) = 
	length ps + cantDePresasTotales l1 + cantDePresasTotales l2 + cantDePresasTotales l3

cantDePresas :: Lobo -> Int
cantDePresas (Cazador _ ps _ _ _) = length ps
cantDePresas _					  = 0

elAlfa :: Manada -> (Nombre, Int)
elAlfa m = (nombreLobo (elMasCazadorManada m), cantDePresas (elMasCazadorManada m))

elMasCazadorManada :: Manada -> Lobo
elMasCazadorManada m = elMasCazador (cazadores m)

elMasCazador :: [Lobo] -> Lobo
elMasCazador [x] = x
elMasCazador (x:xs) = 
	if cantDePresas x > cantDePresas (elMasCazador xs)
		then x
		else elMasCazador xs 

cazadores :: Manada -> [Lobo]
cazadores (M l) = cazadoresL l

cazadoresL :: Lobo -> [Lobo]
cazadoresL (Cria n) = []
cazadoresL (Explorador n ts l1 l2) = cazadoresL l1 ++ cazadoresL l2
cazadoresL (Cazador n ps l1 l2 l3) = 
	(Cazador n ps l1 l2 l3) :
	cazadoresL l1 ++
	cazadoresL l2 ++
	cazadoresL l3

esCazador :: Lobo -> Bool
esCazador (Cria _) = False
esCazador (Explorador _ _ _ _) = False
esCazador (Cazador _ _ _ _ _)  = True

nombreLobo :: Lobo -> Nombre
nombreLobo (Cria n) 		    = n
nombreLobo (Explorador n _ _ _) = n
nombreLobo (Cazador n _ _ _ _)  = n

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronL t l

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cria n) = []
losQueExploraronL t (Explorador n ts l1 l2) = 
	if pertenece t ts
		then n : losQueExploraronL t l1 ++ losQueExploraronL t l2
		else losQueExploraronL t l1 ++ losQueExploraronL t l2
losQueExploraronL t (Cazador n ps l1 l2 l3) =
	losQueExploraronL t l1 ++ 
	losQueExploraronL t l2 ++ 
	losQueExploraronL t l3

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio m = exploradoresPorTerritorioM m (territoriosSinRep m)

exploradoresPorTerritorioM :: Manada -> [Territorio] -> [(Territorio, [Nombre])]
exploradoresPorTerritorioM m [] 		= []
exploradoresPorTerritorioM m (t:ts) = 
	generadorDeTuplas t (losQueExploraron t m) : exploradoresPorTerritorioM m ts 

superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M l) = superioresDelCazadorL n l

superioresDelCazadorL :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorL n (Cria n1) = []
superioresDelCazadorL n (Explorador n1 ts l1 l2) = 
	superioresDelCazadorL n l1 ++ superioresDelCazadorL n l2
superioresDelCazadorL n (Cazador n1 ps l1 l2 l3) = 
	if n == n1
		then []
		else n1 : superioresDelCazadorL n l1 ++ superioresDelCazadorL n l2 ++ superioresDelCazadorL n l3

generadorDeTuplas :: a -> b -> (a, b)
generadorDeTuplas a b = (a, b)

territoriosSinRep :: Manada -> [Territorio]
territoriosSinRep (M l) = territoriosSinRepL l

territoriosSinRepL :: Lobo -> [Territorio]
territoriosSinRepL (Cria n) = []
territoriosSinRepL (Cazador n ps l1 l2 l3) =
    sinRepetidos (territoriosSinRepL l1 ++
        					territoriosSinRepL l2 ++
        					territoriosSinRepL l3)
territoriosSinRepL (Explorador n ts l1 l2) =
    sinRepetidos (ts ++
        					territoriosSinRepL l1 ++
        					territoriosSinRepL l2)


































