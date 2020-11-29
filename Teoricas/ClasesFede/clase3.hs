-- Haskell

-- Modelo funcional para pensar
-- estructuras de datos

-- Funciones
-- Polimorfismo (paramétrico)
    -- id :: a -> a
    -- head :: [a] -> a
-- Pattern Matching
    -- forma de análisis de casos
-- Tipos de datos / Estructuras de Datos
    -- enumerativos
       -- data Color = Rojo | Negro
    -- registros
       -- data Persona = P Int String
    -- tuplas (pares)
        -- (a, b) es lo mismo que
        -- data Par a b = ConsPar a b
    -- Sinonimos de tipos
       -- type DNI = Int
       -- type Nombre = String
       -- type Persona = Par Int String
    -- listas
       -- [a]
       -- type String = [a]
-- Funciones recursivas
    -- Por recursión estructural
    -- length :: [a] -> Int
    -- length []     = 0
    -- length (x:xs) = 1 + length xs

    -- minimo :: [Int] -> Int
    -- minimo (x:[]) = x
    -- minimo (x:xs) = min x (minimo xs)

-- Easter eggs
-- Zekiel Ash
-- fondo de DOOM

-- Lucho Zuccolo
-- RE2 en favoritos

type Nombre = String

dni1 :: DNI
dni1 = 434580123

dni2 :: DNI
dni2 = 128301

dni3 :: DNI
dni3 = 1321290390

pepe :: Nombre
pepe = "Pepe"

minimo :: [Int] -> Int
minimo (x:[]) = x
minimo (x:xs) = min x (minimo xs)

-------------------------------------
type Tramite = Int
type DNI     = Int

          -- caso recursivo
data Fila = Primero DNI Tramite Fila
          -- caso base
          | Nadie

-- Version simplificada
-- data Persona = P DNI Tramite
-- type Fila    = [Persona]

tramite1 :: Tramite
tramite1 = 513


tramite2 :: Tramite
tramite2 = 219

tramite3 :: Tramite
tramite3 = 205

fila1 :: Fila
fila1 =
    Primero dni1 tramite1
     (Primero dni2 tramite2
        (Primero dni3 tramite3 Nadie))

fila2 :: Fila
fila2 = Nadie

fila3 :: Fila
fila3 = Primero dni3 tramite3 Nadie

esFilaVacia :: Fila -> Bool
esFilaVacia Nadie = True
esFilaVacia f     = False

dniDelPrimero :: Fila -> DNI
dniDelPrimero (Primero dni tra fila) = dni

longitudDeLaFila :: Fila -> Int
longitudDeLaFila Nadie = 0
longitudDeLaFila (Primero dni tra fila) =
    1 + longitudDeLaFila fila

tramites :: Fila -> [Tramite]
tramites Nadie = []
tramites (Primero dni tra fila) =
    tra : tramites fila


-- Una variante con otros nombres
-- data Fila = Persona DNI Tramite Fila
--           | FinDeFila

-- tramites :: Fila -> [Tramite]
-- tramites FinDeFila = []
-- tramites (Persona dni tra fila) =
--  tra : tramites fila

---------------------------------------------


data Material = Vidrio | Plastico | Ceramica
              deriving (Show, Eq)

                  -- caso recursivo
data PilaDePlatos = Plato Material PilaDePlatos
                  -- caso base
                  | Mesada
                  deriving Show

pila1 :: PilaDePlatos
pila1 = Plato Vidrio (Plato Plastico Mesada)

pila2 :: PilaDePlatos
pila2 = Mesada

pila3 :: PilaDePlatos
pila3 = Plato Ceramica
           (Plato Plastico 
            (Plato Ceramica Mesada))

-- Similar a
-- type PilaDePlatos = [Material]

tengoAlgoQueLavar :: PilaDePlatos -> Bool
tengoAlgoQueLavar Mesada = False
tengoAlgoQueLavar p      = True

hayUnoDeVidrio :: PilaDePlatos -> Bool
hayUnoDeVidrio Mesada         = False
hayUnoDeVidrio (Plato mat ps) =
    esDeVidrio mat || hayUnoDeVidrio ps

esDeVidrio Vidrio = True
esDeVidrio mat    = False

-- Sin repetidos
materiales :: PilaDePlatos -> [Material]
materiales Mesada         = []
materiales (Plato mat ps) =
         agregarSiNoEsta mat (materiales ps)

agregarSiNoEsta :: Material -> [Material] -> [Material]
agregarSiNoEsta mat mats =
    if pertenece mat mats
       then mats
       else mat : mats

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) =
    e == x || pertenece e xs

-------------------------------------------------
type Chofer = String
type CantPasajeros = Int

data Tren = Cabina Chofer Vagon

            -- caso recursivo
data Vagon  = UnVagon CantPasajeros Vagon
            --- caso base
            | UltimoVagon CantPasajeros

-- Paralelismo con listas
-- [x]
-- (x:xs)

-- UltimoVagon cant
-- UnVagon     cant vs

masDeUnVagon :: Tren -> Bool
masDeUnVagon (Cabina ch vgs) =
    masDeUno vgs

    -- alternativa
    -- cantVagones vgs > 1

-- version eficiente
masDeUno :: Vagon -> Bool
masDeUno (UltimoVagon cant) = False
masDeUno vgs                = True

-- version menos eficiente
cantVagones :: Vagon -> Int
cantVagones (UltimoVagon cant) = 1
cantVagones (UnVagon cant vgs) =
    1 + cantVagones vgs

cantPasajeros :: Tren -> Int
cantPasajeros (Cabina ch vgs) =
    cantPasajerosEnVagones vgs

cantPasajerosEnVagones :: Vagon -> Int
cantPasajerosEnVagones (UltimoVagon cant) = cant
cantPasajerosEnVagones (UnVagon cant vgs) =
    cant + cantPasajerosEnVagones vgs

tren1 :: Tren
tren1 = undefined

tren2 :: Tren
tren2 = undefined

tren3 :: Tren
tren3 = undefined

---------------------------------------------------

-- Podemos crear cualquier tipo que sea
-- similar a las listas

-- data [a] = []
--          | a : [a]

---------------------------------------------------

data ArbolG = PersonaG Nombre ArbolG ArbolG
            | SinInfo
            deriving Show

-- type ArbolG = Tree Nombre

personas :: ArbolG -> [Nombre]
personas SinInfo = [] 
personas (PersonaG n g1 g2) =
    n : (personas g1 ++ personas g2)

estaPersona :: Nombre -> ArbolG -> Bool
estaPersona nm SinInfo            = False
estaPersona nm (PersonaG n g1 g2) =
    nm == n || estaPersona nm g1 || estaPersona nm g2

profundidad :: ArbolG -> Int
profundidad SinInfo            = 0
profundidad (PersonaG n g1 g2) =
    1 + max (profundidad g1) (profundidad g2)

ramaMasLarga :: ArbolG -> [String]
ramaMasLarga SinInfo            = []
ramaMasLarga (PersonaG n g1 g2) =
    if profundidad g1 > profundidad g2
       then n : ramaMasLarga g1
       else n : ramaMasLarga g2

personasEnNivel :: Int -> ArbolG -> [String]
-- personasEnNivel 0 SinInfo         = []
personasEnNivel n SinInfo            = []
personasEnNivel 0 (PersonaG p g1 g2) = [p]
personasEnNivel n (PersonaG p g1 g2) =
  personasEnNivel (n-1) g1 ++ personasEnNivel (n-1) g2

elementoEn :: Int -> [a] -> a
elementoEn 0 []     = error "no hay elementos"
elementoEn 0 (x:xs) = x
elementoEn n []     = error "no hay elementos"
elementoEn n (x:xs) = elementoEn (n-1) xs

--  0    1   2
-- [11, 23, 35]

-- elementoEn 2 [11, 23, 35]
-- -> n = 2, x = 11, xs = [23, 35]
-- elementoEn (2-1) [23, 35]
-- ->
-- elementoEn 1 [23, 35]
-- -> n = 1, x = 23, xs = [35]
-- elementoEn (1-1) [35]
-- ->
-- elementoEn 0 [35]
-- -> 0 == 0, x = 35, xs = []
-- 35

-- elementoEn 0 [11, 23, 35]
-- ->
-- 11

-- elementoEn 3 []
-- ->
-- error "no hay elementos"

arbolg1 :: ArbolG
arbolg1 =
    PersonaG "jorgito"
        (PersonaG "maria teresa"
            SinInfo
            SinInfo)
        (PersonaG "ruben del espacio"
            SinInfo
            SinInfo)

arbolg2 :: ArbolG
arbolg2 = 
    PersonaG "Goku"
        (PersonaG "Bardock"
            (PersonaG "Abuelo Gohan"
                SinInfo
                SinInfo)
            SinInfo)
        SinInfo

arbolg3 :: ArbolG
arbolg3 =
    PersonaG "Roshi" SinInfo SinInfo

arbolg4 :: ArbolG
arbolg4 =
    PersonaG "Pan"
        (PersonaG "Gohan"
            (PersonaG "Milk"
                (PersonaG "Ox Satan"
                    SinInfo
                    SinInfo)
                SinInfo)
            arbolg2)
        (PersonaG "Videl"
            (PersonaG "Mister Satan"
                SinInfo
                SinInfo)
            SinInfo)

data Tree a = NodeT a (Tree a) (Tree a)
            | EmptyT
            deriving Show

sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT x ti td) =
    1 + sizeT ti + sizeT td

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x ti td) =
    x : (toList ti ++ toList td)

t1 :: Tree Int
t1 = NodeT 32
        (NodeT 35
          (NodeT 100
              (NodeT 15
                  EmptyT
                  EmptyT)
              EmptyT)
          EmptyT)
        (NodeT 40
          (NodeT 50
              EmptyT
              EmptyT)
          EmptyT)