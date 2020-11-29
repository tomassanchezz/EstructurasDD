-- 1 -----------------------------------------------------------------------------------
-- 1a) Dado un número devuelve su sucesor
sucesor :: Int -> Int
sucesor x = x + 1

-- 1b) Dados dos numeros devuelve su suma utilizando la operacion +
sumar :: Int -> Int -> Int
sumar x y = x + y

-- 1c) Dado dos números, devuelve un par donde la primera componente es la división del
-- primero por el segundo, y la segunda componente es el resto de dicha división.
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto a b = (div a b , mod a b)

-- 1d) Dado un par de números devuelve el mayor de estos.
maxDelPar :: (Int, Int) -> Int
maxDelPar (a, b) = 
    if a > b
        then a
        else b

-- 2 -----------------------------------------------------------------------------------
-- 1a) Dada una dirección devuelve su opuesta.
data Dir = Norte | Sur | Este | Oeste deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

-- 1b) Dadas dos direcciones, indica si son la misma.
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

-- 1c) Dada una dirección devuelve su siguiente, en sentido horario,
-- y suponiendo que no existe la siguiente dirección a Oeste
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "no existe direccion siguiente a Oeste"

-- 2a) Devuelve un par donde la primera componente es 
-- el primer día de la semana, y la
-- segunda componente es el último día de la semana.
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

-- 2b) Dado un dia de la semana indica si comienza con la letra M 
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

-- 2c) Dado dos dias de semana, indica si el primero viene despues del
-- segundo.
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues Lunes Domingo = True
vieneDespues _ _ = False

-- 2d) Dado un dia de la semana indica si no es ni el primero ni el ultimo.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

-- 3a) Dado un booleano, si es True devuelve False, y si es False devuelve True.
negar :: Bool -> Bool
negar True = False
negar False = True

-- 3b) Dados dos booleanos, si el primero es True y el segundo es False,
-- devuelve False, sino devuelve True
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

-- 3c) Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

-- 3d) Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
or :: Bool -> Bool -> Bool
or True _ = True
or _ True = True
or _ _ = False

-- 3 -----------------------------------------------------------------------------------
-- 1) 
data Persona = ConsP String Int
    deriving Show

nombre :: Persona -> String
nombre (ConsP n e) = n

edad :: Persona -> Int
edad (ConsP n e) = e 

crecer :: Persona -> Persona
crecer (ConsP n e) = ConsP n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevo (ConsP n e) = ConsP nuevo e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = 
    if esMayorQueLaOtra p1 p2
        then p1 
        else p2 

-- 2)
data TipoDePokemon = Agua | Fuego | Planta deriving (Show, Eq)
data Entrenador = ConsEnt String Pokemon Pokemon deriving Show
data Pokemon = ConsPoke TipoDePokemon Int deriving Show

--------------------------------------------
charizard :: Pokemon
charizard = ConsPoke Fuego 100

bulbasaur :: Pokemon
bulbasaur = ConsPoke Planta 100

ash :: Entrenador
ash = ConsEnt "Ash" charizard bulbasaur

tom :: Entrenador
tom = ConsEnt "Tom" charizard bulbasaur
--------------------------------------------

tipo :: Pokemon -> TipoDePokemon
tipo (ConsPoke t en) = t

leGanaA :: TipoDePokemon -> TipoDePokemon
leGanaA Agua      = Fuego
leGanaA Planta    = Agua
leGanaA Fuego     = Planta

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = leGanaA (tipo p1) == tipo p2 

cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonesDe t (ConsEnt s pok1 pok2) = unoSiEsTipo t pok1 + unoSiEsTipo t pok2

unoSiEsTipo :: TipoDePokemon -> Pokemon -> Int
unoSiEsTipo t (ConsPoke tipo en) = 
    if esMismoTipo t tipo
        then 1
        else 0

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua Agua = True
esMismoTipo Fuego Fuego = True
esMismoTipo Planta Planta = True
esMismoTipo _ _ = False

juntarPokemones :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemones (e1, e2) = (pokemonesDe e1) ++ (pokemonesDe e2)

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (ConsEnt s pok1 pok2) = [pok1, pok2] 


-- 4 -----------------------------------------------------------------------------------
-- 1)a)
loMismo :: a -> a
loMismo a = a

-- 1)b) Dado un elemento de algun tipo devuelve siempre 7.
siempreSiete :: a -> Int
siempreSiete a = 7

-- 1c) 
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- 5 -----------------------------------------------------------------------------------

--5a) isEmpty :: [a] -> Bool
--Dada una lista de elementos, si es vacia devuelve True, sino de vuelve False. Definida en haskell como null.

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty xs = False

--5b) head' :: [a] -> a
--Dada una lista devuelve su primer elemento. Definida en Haskell como head.

head' :: [a] -> a
head' [] = error "necesito al menos un elemento"
head' (x:xs) = x

--5c) tail' :: [a] -> [a]
--Dada una lista devuelve esa lista menos el primer elemento. Definida en Haskell como tail.

tail' :: [a] -> [a]
tail' [] = error "necesito al menos dos elementos"
tail' (x:y:xs) = y:xs

--5d)
splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs) 


    












