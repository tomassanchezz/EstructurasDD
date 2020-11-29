--Recursion sobre listas----------------------------------------------------

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = (x + 1) : sucesores xs

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x (ys) 

apariciones :: Eq a => a -> [a] -> Int
apariciones x [] = 0
apariciones x (y:ys) = 
    if x == y
        then 1 + apariciones x ys
        else apariciones x ys

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA x [] = []
losMenoresA x (y:ys) = 
    if x > y
        then y : losMenoresA x ys
        else losMenoresA x ys

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA x [] = []
lasDeLongitudMayorA x (y:ys) = 
    if length y > x 
        then y : lasDeLongitudMayorA x ys
        else lasDeLongitudMayorA x ys

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] a = [a]
agregarAlFinal xs a = xs ++ [a]

concatenar :: [a] -> [a] -> [a]
concatenar [] [] = []
concatenar xs [] = xs
concatenar [] xs = xs
concatenar xs (y:ys) = concatenar (agregarAlFinal xs y) ys

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos xs [] = xs
zipMaximos [] xs = xs
zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys

elMinimo :: Ord a => [a] -> a
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)

--Recursion sobre numeros---------------------------------------------------

factorial :: Int -> Int 
factorial 0 = 1
factorial x = x * factorial (x-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva x = 
    if x < 1
        then []
        else x : cuentaRegresiva (x-1)

repetir :: Int -> a -> [a]
repetir 0 e = []
repetir x e = e : repetir (x-1) e 

losPrimeros :: Int -> [a] -> [a]
losPrimeros n [] = []
losPrimeros n (x:xs) = 
    if n > 0 
        then x : losPrimeros (n-1) xs
        else []

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros n [] = []
sinLosPrimeros n (x:xs) = 
    if n > 1 
        then sinLosPrimeros (n-1) xs
        else xs

--Registros------------------------------------------------------------------
data Persona = Persona String Int 
    deriving Show

tom :: Persona
tom = Persona "Tomas" 21

nico :: Persona
nico = Persona "Nicolas" 20

sino :: Persona
sino = Persona "Francisco" 23

villal :: Persona
villal = Persona "Joaquin" 21

personas :: [Persona]
personas = [tom, nico, sino, villal]

edad :: Persona -> Int
edad (Persona n e) = e

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (x:xs) = 
    if edad x > n
        then x : mayoresA n xs
        else mayoresA n xs

promedioEdad :: [Persona] -> Int
promedioEdad [x] = edad x 
promedioEdad xs =  div (sumEdades xs) (length xs)

sumEdades :: [Persona] -> Int
sumEdades [] = 0
sumEdades (x:xs) = edad x + sumEdades xs

elMasViejo :: [Persona] -> Persona
elMasViejo [x] = x
elMasViejo (x:xs) = elMayor x (elMasViejo xs)

elMayor :: Persona -> Persona -> Persona
elMayor p1 p2 = 
    if edad p1 > edad p2
        then p1
        else p2
-----------------------------------------------------------------------
data TipoDePokemon = Agua | Fuego | Planta deriving (Show, Eq)
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]
---------------------------------------------------------------
ash :: Entrenador
ash = ConsEntrenador "Ash" [bulbasour, charizard, squirtle]

tomas :: Entrenador 
tomas = ConsEntrenador "Tom" [pikachu, eevee, julio]

bulbasour :: Pokemon
bulbasour = ConsPokemon Planta 90

charizard :: Pokemon
charizard = ConsPokemon Fuego 115

squirtle :: Pokemon
squirtle = ConsPokemon Agua 100

pikachu :: Pokemon
pikachu = ConsPokemon Fuego 80

eevee :: Pokemon
eevee = ConsPokemon Planta 110

julio :: Pokemon
julio = ConsPokemon Agua 120
--------------------------------------------------------------
tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon t e) = t

pokemones :: Entrenador -> [Pokemon]
pokemones (ConsEntrenador n pks) = pks

cantPokemones :: Entrenador -> Int
cantPokemones ent = length (pokemones ent)

cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonesDe t (ConsEntrenador n xs) = cantPokemonesDeL t xs

cantPokemonesDeL :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonesDeL t [] = 0
cantPokemonesDeL t (x:xs) = 
    if tipo x == t
        then 1 + cantPokemonesDeL t xs
        else cantPokemonesDeL t xs

leGanaA :: TipoDePokemon -> TipoDePokemon -> Bool
leGanaA Agua Fuego   = True
leGanaA Fuego Planta = True 
leGanaA Planta Agua  = True
leGanaA _ _          = False 

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan t (ConsEntrenador n pks) (ConsEntrenador n1 pks1) = leGanaATodosYEsDeTipo t pks pks1

leGanaATodosYEsDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
leGanaATodosYEsDeTipo t (p:pks) pks1 = 
    if leGanaATodos p pks1 && esTipo t p 
        then 1 + leGanaATodosYEsDeTipo t pks pks1
        else leGanaATodosYEsDeTipo t pks pks1

esTipo :: TipoDePokemon -> Pokemon -> Bool
esTipo Agua (ConsPokemon Agua _)     = True
esTipo Fuego (ConsPokemon Fuego _)   = True
esTipo Planta (ConsPokemon Planta _) = True
esTipo _ (ConsPokemon _ _ )          = False

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos p1 []     = True
leGanaATodos p1 (p:ps) =
	leGanaA (tipo p1) (tipo p) && leGanaATodos p1 ps

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador n xs) = unoDeCada xs

unoDeCada :: [Pokemon] -> Bool
unoDeCada [] = False
unoDeCada xs = 
    cantPokemonesDeL Agua xs >= 1 && 
    cantPokemonesDeL Fuego xs >= 1 &&
    cantPokemonesDeL Planta xs >= 1 
---------------------------------------------------------------------------
data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving (Show, Eq)
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto 
data Empresa = ConsEmpresa [Rol]

empresa1 :: Empresa
empresa1 = ConsEmpresa [dev1, man1, dev2]

dev1 :: Rol
dev1 = Developer SemiSenior pro2

dev2 :: Rol
dev2 = Developer Junior pro1

man1 :: Rol
man1 = Management Senior pro1

pro1 :: Proyecto
pro1 = ConsProyecto "Testing"

pro2 :: Proyecto
pro2 = ConsProyecto "NicoForroHdp"

lsRoles :: [Rol]
lsRoles = [dev1, dev2, man1]

proyecto :: Rol -> Proyecto
proyecto (Developer s p)  = p
proyecto (Management s p) = p

seniority :: Rol -> Seniority
seniority (Developer s p)  = s
seniority (Management s p) = s

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa roles) = sinRepetidos (proyectosR roles)

roles :: Empresa -> [Rol]
roles (ConsEmpresa rs) = rs

proyectosR :: [Rol] -> [Proyecto]
proyectosR []     = []
proyectosR (x:xs) = proyecto x : proyectosR xs

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (filter (/= x) xs)

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) [] = 0
losDevSenior (ConsEmpresa rs) ps = losDevSeniorEnPs rs ps   

losDevSeniorEnPs :: [Rol] -> [Proyecto] -> Int
losDevSeniorEnPs rs [] = 0
losDevSeniorEnPs [] ps = 0
losDevSeniorEnPs (r:rs) ps = 
    if esSeniorYPerteneceAProyecto r ps
        then 1 + losDevSeniorEnPs rs ps
        else losDevSeniorEnPs rs ps

esSenior :: Rol -> Bool
esSenior (Developer Senior _)  = True
esSenior (Management Senior _) = True
esSenior _                     = False

esSeniorYPerteneceAProyecto :: Rol -> [Proyecto] -> Bool
esSeniorYPerteneceAProyecto r ps = 
    esSenior r && pertenece (proyecto r) ps

cantQueTrabajaEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajaEn ps (ConsEmpresa rs) = losQueTrabajanEn ps rs

losQueTrabajanEn :: [Proyecto] -> [Rol] -> Int
losQueTrabajanEn [] (r:rs) = 0
losQueTrabajanEn ps []     = 0
losQueTrabajanEn ps (r:rs) = 
    if pertenece (proyecto r) ps
        then 1 + losQueTrabajanEn ps rs
        else losQueTrabajanEn ps rs

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto emp = empleadosPorProyecto (proyectos emp) (roles emp)

empleadosPorProyecto :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
empleadosPorProyecto [] [] = []
empleadosPorProyecto ps [] = []
empleadosPorProyecto [] rs = []
empleadosPorProyecto (p:ps) rs = 
    generadorDeTuplas p (empleadosDeUnProyecto rs p) : empleadosPorProyecto ps rs

generadorDeTuplas :: a -> b -> (a, b)
generadorDeTuplas a b = (a, b)

empleadosDeUnProyecto :: [Rol] -> Proyecto -> Int
empleadosDeUnProyecto [] p     = 0
empleadosDeUnProyecto (r:rs) p = 
    if proyecto r == p
        then 1 + empleadosDeUnProyecto rs p
        else empleadosDeUnProyecto rs p

