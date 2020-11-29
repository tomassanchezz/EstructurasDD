-- Ejercicio 1
---------------------------------------------------------

data Pizza = Prepizza
           | Capa Ingrediente Pizza
           deriving (Show)

data Ingrediente =
    Salsa
  | Queso
  | Jamon
  | Aceitunas Int
  deriving (Show)

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing ps) =
  1 + cantidadDeCapas ps

ingredientes :: Pizza -> [Ingrediente]
ingredientes Prepizza = []
ingredientes (Capa ing ps) =
  ing : ingredientes ps

soloQueso :: Pizza -> Bool
soloQueso Prepizza = True
soloQueso (Capa ing ps) =
  esQueso ing && soloQueso ps

-- NO está bueno
-- if esQueso ing
--    then soloQueso ps
--    else False

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

soloAceitunas :: Pizza -> Bool
soloAceitunas Prepizza = True
soloAceitunas (Capa ing ps) =
  esAceitunas ing && soloAceitunas ps

esAceitunas :: Ingrediente -> Bool
esAceitunas (Aceitunas _) = True
esAceitunas _ = False

sacarAceitunas :: Pizza -> Pizza
sacarAceitunas Prepizza = Prepizza
sacarAceitunas (Capa ing ps) =
  if esAceitunas ing
     then sacarAceitunas ps
     else Capa ing (sacarAceitunas ps)

cuantasCapasTieneJamon :: Pizza -> Int
cuantasCapasTieneJamon Prepizza = 0
cuantasCapasTieneJamon (Capa ing ps) =
  if esJamon ing
     then 1 + cuantasCapasTieneJamon ps
     else cuantasCapasTieneJamon ps

-- Version casi difficult
-- (if es jamon ing then 1 else 0) + cuantasCapas ps     

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

cantIngNoAceituna :: Pizza -> [(Ingrediente, Int)]
cantIngNoAceituna Prepizza = []
cantIngNoAceituna (Capa ing ps) =
  sumarIng ing (cantIngNoAceituna ps)

-- Ejemplos de lo que devuelve la recursión
-- []

-- [(Queso, 3), (Jamon, 2)]

-- Si ya está le sumo uno

-- Y si no está hay que agregarlo

sumarIng :: Ingrediente 
         -> [(Ingrediente, Int)]
         -> [(Ingrediente, Int)]
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

-- fst (x, y) = x
-- snd (x, y) = y

-- Cristian Gonzalez
-- Maná

ejemplo1 :: Pizza
ejemplo1 = Capa Jamon (Capa Queso (Capa Salsa Prepizza))

ejemplo2 :: Pizza
ejemplo2 = Capa (Aceitunas 8) (Capa Queso Prepizza)

ejemplo3 :: Pizza
ejemplo3 = Prepizza

ejemplo4 :: Pizza
ejemplo4 = Capa Queso (Capa Jamon (Capa Queso Prepizza))

ejemplo5 :: Pizza
ejemplo5 = Capa Queso (Capa Queso Prepizza)

---------------------------------------------------------

--- Ejercicio 2
---------------------------------------------------------

data Dir = Izq | Der deriving Show

data Objeto = Tesoro | Chatarra deriving Show

data Cofre = Cofre [Objeto] deriving Show

data Mapa = Fin Cofre
          | Bifurcacion Cofre Mapa Mapa
          deriving Show

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = tieneTesoro c
hayTesoro (Bifurcacion c m1 m2) =
  tieneTesoro c || hayTesoro m1 || hayTesoro m2

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre objs) = algunoEsTesoro objs

algunoEsTesoro :: [Objeto] -> Bool
algunoEsTesoro []     = False
algunoEsTesoro (o:os) =
  esTesoro o || algunoEsTesoro os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

mapa1 :: Mapa
mapa1 = Fin (Cofre [Chatarra])

mapa2 :: Mapa
mapa2 = 
  Bifurcacion (Cofre [Chatarra])
              (Fin (Cofre [Tesoro]))
              (Fin (Cofre [Tesoro]))

mapa3 :: Mapa
mapa3 = Bifurcacion (Cofre [Chatarra]) 
                    mapa1
                    mapa1

mapa4 :: Mapa
mapa4 = Bifurcacion (Cofre [Tesoro])
                    mapa3
                    mapa3

-- Tomas Sanchez
-- Ricky Martin

cantChatarra :: Mapa -> Int
cantChatarra (Fin c) = cantChatarraEnCofre c
cantChatarra (Bifurcacion c m1 m2) =
  cantChatarraEnCofre c + cantChatarra m1 + cantChatarra m2

cantChatarraEnCofre :: Cofre -> Int
cantChatarraEnCofre (Cofre objs) = cuantoObjChatarra objs

cuantoObjChatarra :: [Objeto] -> Int
cuantoObjChatarra [] = 0
cuantoObjChatarra (x:xs) =
  if esChatarra x
  then 1 + cuantoObjChatarra xs
  else cuantoObjChatarra xs

esChatarra :: Objeto -> Bool
esChatarra Chatarra = True
esChatarra _        = False


-- Devuelve un mapa igual pero sin chatarra
sinChatarra :: Mapa -> Mapa
sinChatarra (Fin c) = Fin (cofreSinChatarra c)
sinChatarra (Bifurcacion c m1 m2) =
  Bifurcacion (cofreSinChatarra c)
              (sinChatarra m1)
              (sinChatarra m2)

cofreSinChatarra :: Cofre -> Cofre
cofreSinChatarra (Cofre objs) = Cofre (objsSinChatarra objs)

objsSinChatarra :: [Objeto] -> [Objeto]
objsSinChatarra [] = []
objsSinChatarra (x:xs) =
  if esChatarra x
     then objsSinChatarra xs
     else x : objsSinChatarra xs

-- Devuelve los objetos de todos los cofres
-- Nota: con repetidos
objetos :: Mapa -> [Objeto]
objetos (Fin c) = objetosDeCofre c
objetos (Bifurcacion c m1 m2) =
  objetosDeCofre c
  ++
  objetos m1
  ++
  objetos m2

objetosDeCofre :: Cofre -> [Objeto]
objetosDeCofre (Cofre objs) = objs

-- Prec.: la lista apunta a una parte existen del mapa
cantChatarraEn :: [Dir] -> Mapa -> Int
cantChatarraEn [] (Fin c) = cantChatarraEnCofre c
cantChatarraEn [] (Bifurcacion c m1 m2) = cantChatarraEnCofre c
cantChatarraEn (d:ds) (Fin c) = error "se termino el camino y la lista no"
cantChatarraEn (d:ds) (Bifurcacion c m1 m2) = 
  if esIzq d
     then cantChatarraEn ds m1
     else cantChatarraEn ds m2

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False

--------------------------------------------------------

type Presa = String
type Territorio = String
type Nombre = String

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cria Nombre

cantCazadores :: Lobo -> Int
cantCazadores (Cria n) = 0
cantCazadores (Cazador n ps l1 l2 l3) =
  1 + cantCazadores l1 + cantCazadores l2 + cantCazadores l3
cantCazadores (Explorador n ts l1 l2) =
  cantCazadores l1 + cantCazadores l2

territorios :: Lobo -> [Territorio]
territorios (Cria n) = []
territorios (Cazador n ps l1 l2 l3) =
    territorios l1 ++
    territorios l2 ++
    territorios l3
territorios (Explorador n ts l1 l2) =
    ts ++
    territorios l1 ++
    territorios l2

cuantasCriasTieneN :: Nombre -> Lobo -> Int
cuantasCriasTieneN nom (Cria n) = 0
cuantasCriasTieneN nom (Cazador n ps l1 l2 l3) =
  if nom == n
     then cantCrias l1 + cantCrias l2 + cantCrias l3
     else cuantasCriasTieneN nom l1 +
          cuantasCriasTieneN nom l2 +
          cuantasCriasTieneN nom l3

cuantasCriasTieneN nom (Explorador n ps l1 l2) =
  if nom == n
    then cantCrias l1 + cantCrias l2
    else cuantasCriasTieneN nom l1 + cuantasCriasTieneN nom l2

cantCrias :: Lobo -> Int
cantCrias (Cria n) = 1
cantCrias (Cazador n ps l1 l2 l3) = 
  cantCrias l1 + cantCrias l2 + cantCrias l3
cantCrias (Explorador n ts l1 l2) = 
  cantCrias l1 + cantCrias l2

-----------------------------------------------------

data Componente = LanzaTorpedos
                | Motor Int
                | Almacen [Barril]
                deriving Show

data Barril = Comida
            | Oxigeno
            | Torpedo
            | Combustible
            deriving (Show, Eq)

data Sector = S SectorId [Componente] [Tripulante] deriving Show

type SectorId = String
type Tripulante = String

data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a)
            deriving Show

data Nave = N (Tree Sector) deriving Show

sectores :: Nave -> [SectorId]
sectores (N t) = sectoresArbol t

sectoresArbol :: Tree Sector -> [SectorId]
sectoresArbol EmptyT = []
sectoresArbol (NodeT x t1 t2) =
  idDeSector x : (sectoresArbol t1 ++ sectoresArbol t2)

idDeSector :: Sector -> SectorId
idDeSector (S idS _ _) = idS

cantComida :: Sector -> Int
cantComida (S _ comps _) = cantCompQTieneC comps

cantCompQTieneC :: [Componente] -> Int
cantCompQTieneC [] = 0  
cantCompQTieneC (x:xs) =
  cantComidaEnC x + cantCompQTieneC xs

cantComidaEnC :: Componente -> Int
cantComidaEnC (Almacen bs) = apariciones Comida bs
cantComidaEnC _            = 0

-- No hace falta porque Barril implementa eq
-- cantComidaEnBS :: [Barril] -> Int
-- cantComidaEnBS [] = 0
-- cantComidaEnBS (x:xs) =
--   if esComida x
--     then 1 + cantComidaEnBS xs
--     else cantComidaEnBS xs

apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) =
  if e == x
     then 1 + apariciones e xs
     else apariciones e xs
-- Para que sea redondo