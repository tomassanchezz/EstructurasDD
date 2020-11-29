-- - Tipos Algebraicos
--   - Pattern Matching para analizar casos
--   - Como ejemplo enumerativos y registros
--   - También vimos tuplas como primera estructura de datos
--   - Vimos un poco sobre cómo es su estructura

-- En Haskell todo es una expresión
-- 	 - Ejecutar el programa consiste en evaluar una expresión
--   - No puedo usar comandos para recorrer estructuras
--   - foreach, for, while, if-statement, cualquier forma de estructura de control


-- Es parcial
head' :: [a] -> a
head' (x:xs) = x

-- Es parcial
tail' :: [a] -> [a]
tail' (x:xs) = xs

-- Es total
null' :: [a] -> Bool
null' [] = True
null' xs = False

-- Queremos que sea total
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- sumatoria (1 : (2 : (3 : [])))
-- -> -- x = 1, xs = (2 : (3 : []))
-- 1 + sumatoria (2 : (3 : []))
-- -> -- x = 2, xs = (3 : [])
-- 1 + (2 + sumatoria (3 : []))
-- -> -- x = 3, xs = []
-- 1 + (2 + (3 + sumatoria []))
-- -> -- caso base
-- 1 + (2 + (3 + 0))
-- -> -- 3 + 0 = 3
-- 1 + (2 + 3)
-- -> -- 2 + 3 = 5
-- 1 + 5
-- -> -- 1 + 5 = 6
-- 6

-- [1,2,3] == (1 : 2 : 3 : [])

-- Argumento (input)
-- (1 : 2 : 3 : [])

-- Resultado
-- (1 + (2 + 3 + 0))

todoTrue :: [Bool] -> Bool
todoTrue []     = True
todoTrue (x:xs) = x && todoTrue xs

-- hayAlMenosUn5 :: [Int] -> Bool
-- hayAlMenosUn5 []     = False
-- hayAlMenosUn5 (x:xs) = x == 5 || hayAlMenosUn5 xs

hayAlMenosUn5 :: [Int] -> Bool
hayAlMenosUn5 xs = hayAlMenosUnN 5 xs

hayAlMenosUnN :: Int -> [Int] -> Bool
hayAlMenosUnN n []     = False
hayAlMenosUnN n (x:xs) = x == n || hayAlMenosUnN n xs

-- True : False : True : []
-- ->
-- ...
-- ->
-- True && False && True && True

-- True  && True = True
-- False && _   = False
-- _     && False = False

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-- 1 : 2 : 3 : []
-- ->
-- 1 + 1 + 1 + 0

-- Prec.: la lista no está vacía
elMinimo :: [Int] -> Int
elMinimo (x:[]) = x
elMinimo (x:xs) = min x (elMinimo xs)

-- Prec.: la lista no está vacía
elMaximo :: [Int] -> Int
elMaximo []     = error "no existe un elemento maximo en una lista vacia"
elMaximo (x:[]) = x
elMaximo (x:xs) = max x (elMaximo xs)

-- elMaximo (1 : (-10) : 3 : [])
-- -> x = 1, xs = (-10) : 3 : []
-- max 1 (elMaximo ((-10) : 3 : []))
-- -> x = -10, xs = 3 : []
-- max 1 (max (-10) (elMaximo (3 : [])))
-- -> x = 3
-- max 1 (max (-10) 3)
-- -> max (-10) 3 = 3
-- max 1 3
-- -> max 1 3 = 3
-- 3

-- elMaximo (1 : 2 : 3 : [])
-- ->
-- ...
-- ->
-- max 1 (max 2 3)
-- ->
-- ...
-- ->
-- 3

-- max n m = if n > m
-- 	         then n
-- 	         else m

soloLosMayoresAN :: Int -> [Int] -> [Int]
soloLosMayoresAN n []     = []
soloLosMayoresAN n (x:xs) =
	if x > n
	   then x : soloLosMayoresAN n xs
	   else soloLosMayoresAN n xs

-- soloLosMayoresAN 2 (1 : 2 : 3 : [])
-- -> n = 2, x = 1, xs = (2 : 3 : [])
-- soloLosMayoresAN n (2 : 3 : [])
-- -> n = 2, x = 2, xs = (3 : [])
-- soloLosMayoresAN 2 (3 : [])
-- -> n = 2, x = 3, xs = []
-- 3 : soloLosMayoresAN 2 []
-- -> n = 2
-- 3 : []

porDos :: [Int] -> [Int]
porDos []     = []
porDos (x:xs) = (x * 2) : porDos xs

-- porDos (3:4:[])
-- ->
-- ...
-- ->
-- (3 * 2) : (4 * 2) : []

aplanar :: [[a]] -> [a]
aplanar []     = []
aplanar (x:xs) =
	x ++ aplanar xs

lasDeLongitudMenorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMenorA n []     = []
lasDeLongitudMenorA n (x:xs) =
	if longitud x < n
	   then x : lasDeLongitudMenorA n xs
	   else lasDeLongitudMenorA n xs

-- sumatoria :: [Int] -> Int
-- cuentaRegresiva :: Int -> [Int]

sumatoriaN :: Int -> Int
sumatoriaN n = sumatoria (cuentaRegresiva n)

-- sumatoriaN :: Int -> Int
-- sumatoriaN 0 = 0
-- sumatoriaN n = n + sumatoriaN (n - 1)

-- sumatoriaN 5
-- ->
-- ...
-- ->
-- 5 + 4 + 3 + 2 + 1 + 0

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- factorial 5
-- ->
-- ...
-- ->
-- 5 * 4 * 3 * 2 * 1 * 1

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = [0]
cuentaRegresiva n = 
	n : cuentaRegresiva (n - 1)

-- cuentaRegresiva 5
-- ->
-- ...
-- ->
-- 5 : 4 : 3 : 2 : 1 : 0

repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e =
	e : repetir (n-1) e

-- repetir 5 "hola"
-- ->
-- ...
-- ->
-- ["hola", "hola", "hola", "hola", "hola"]

-- longitud (repetir n x)
-- ->
-- ...
-- ->
-- n

data Persona = ConsP Int String

pepe   = ConsP 35380748 "Pepe"
german = ConsP 39148590 "German"
estefania = ConsP 25145862 "Estefania"
grupote = [pepe, german, estefania]

nombres :: [Persona] -> [String]
nombres [] = []
nombres (x:xs) =
	nombre x : nombres xs

nombre :: Persona -> String
nombre (ConsP dni nom) = nom

dnis :: [Persona] -> [Int]
dnis [] = []
dnis (x:xs) =
	dni x : dnis xs

dni :: Persona -> Int
dni (ConsP d nom) = d

maximoDNI :: [Persona] -> Int
maximoDNI xs = elMaximo (dnis xs)

data TipoDePokemon = Agua | Fuego | Planta

data Pokemon = ConsPokemon String TipoDePokemon

bolbasor   = ConsPokemon "bolbasor" Planta
squirtle   = ConsPokemon "squartel" Agua
charizard  = ConsPokemon "charizard" Fuego
charmeleon = ConsPokemon "charmeleon" Fuego

data Entrenador = ConsEntrenador String [Pokemon]

ashMayonesa = ConsEntrenador "Ash" [bolbasor, squirtle]
miguelElFogoso = ConsEntrenador "Miguel" [charizard, charmeleon]

-- D Esc Ctrl Alt

-- Proposito: es True cuando el primer
--            entrenador tiene al menos un
--            pokemon que le gana a todos los
--            pokemon del segundo entrenador
loLiquidaAlOtro :: Entrenador -> Entrenador -> Bool
loLiquidaAlOtro (ConsEntrenador nom1 pokemons1)
                (ConsEntrenador nom2 pokemons2) =
   unoEsSuperiorATodos pokemons1 pokemons2

unoEsSuperiorATodos :: [Pokemon] -> [Pokemon] -> Bool
unoEsSuperiorATodos []     pokes2 = False
unoEsSuperiorATodos (x:xs) pokes2 = 
	(leGanaATodos x pokes2) || (unoEsSuperiorATodos xs pokes2)

leGana :: TipoDePokemon -> TipoDePokemon -> Bool
leGana Agua Fuego    = True
leGana Fuego Planta  = True
leGana Planta Agua   = True
leGana t1     t2     = False

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos p1 []     = True
leGanaATodos p1 (p:ps) =
	   leGana (tipoDe p1) (tipoDe p)
	&& leGanaATodos p1 ps

-- leGanaA :: Pokemon -> Pokemon -> Bool
-- leGanaA poke1 poke2 = leGanaA (tipoDe poke1)
--                               (tipoDe poke2)

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon nom tipo) = tipo