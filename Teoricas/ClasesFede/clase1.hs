doble :: Int -> Int
doble x = x * 2

mult :: Int -> Int -> Int
mult x y = x * y

-- Tipos de datos enumerativos
data Color = Rojo | Negro | Azul | Verde
     deriving Show
data Dir   = Norte | Sur | Este | Oeste
     deriving Show

-- Si no pongo deriving Show, me surge este error al mostrar datos:
-- ERROR - Cannot find "show" function for:
-- *** Expression : Rojo
-- *** Of type    : Color

-- Si le paso un argumento del tipo incorrecto:
-- Main> opuesto Rojo
-- ERROR - Type error in application
-- *** Expression     : opuesto Rojo
-- *** Term           : Rojo
-- *** Type           : Color
-- *** Does not match : Dir

-- Total/Exhaustiva porque define una ecuación por cada caso
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Este  = Oeste
opuesto Oeste = Este

-- Si me olvido de algún caso, la función es parcial

-- Se da cuenta que algo es un constructor porque empieza con mayúscula
-- Main> Amarillo
-- ERROR - Undefined data constructor "Amarillo"
-- Main> amarillo
-- ERROR - Undefined variable "amarillo"

-- El mecanismo de chequeo de coincidencia de constructores
-- se conoce como "Pattern Matching"

-- ¿Total o Parcial?
-- Total porque cubre todos los casos
esRojo :: Color -> Bool
esRojo Rojo = True
esRojo c    = False

-- Se empiezan a chequear coincidencias desde la primera
-- hasta la última ecuación

-- Esto sería algo indeseado
-- Porque siempre coincide con la primera ecuación
-- que es general para todos los colores
-- esRojo :: Color -> Bool
-- esRojo c    = False
-- esRojo Rojo = True

-- Consejo: primero lo especifico, luego lo general

-- No puedo usar == porque para eso
-- tengo que dar un algoritmo que me permita
-- comparar cosas de ese tipo
-- esAzul :: Color -> Bool
-- esAzul c = c == Azul

    -- Intentar no hacer esto,
    -- lo llamamos miedo al booleano
	-- if c == Azul
	--    then True
	--    else False

data TipoDePokemon = Fuego | Agua | Planta | Electrico
     deriving Show

leGanaA :: TipoDePokemon -> TipoDePokemon
leGanaA Agua      = Fuego
leGanaA Planta    = Agua
leGanaA Fuego     = Planta
leGanaA Electrico = Agua

-- Tipos de datos como Registros
data Persona = ConsP String Int
     deriving Show

seLlamaJorge :: Persona -> Bool
seLlamaJorge (ConsP "Jorge" e) = True
seLlamaJorge p                 = False

tieneHistoria :: Persona -> Bool
tieneHistoria (ConsP "Lola" e) = True
tieneHistoria p                = False

edad :: Persona -> Int
edad (ConsP nm e) = e

nombre :: Persona -> String
nombre (ConsP nm e) = nm

esMayorDeEdad :: Persona -> Bool
esMayorDeEdad p = edad p >= 18

-- Otra opción
-- esMayorDeEdad (ConsP nm e) = e >= 18

esJorgeYEsMayor :: Persona -> Bool
esJorgeYEsMayor p = seLlamaJorge p && esMayorDeEdad p

--                      especie % vida  
data Pokemon = ConsPoke String  Int     TipoDePokemon
     deriving Show

pikachu :: Pokemon
pikachu = ConsPoke "pikachu" 100 Electrico

bulbasaur :: Pokemon
bulbasaur = ConsPoke "bulbasaur" 100 Planta

-- Parcial
-- Precondición: n <= vida (sino le dejo vida negativa)
restarVida :: Int -> Pokemon -> Pokemon
restarVida n (ConsPoke esp vida tipo) =
	if n <= vida
	   then ConsPoke esp (vida - n) tipo
	   else error "Estas restando mas vida que la posible"

estaVivo :: Pokemon -> Bool
estaVivo (ConsPoke esp vida tipo) = vida > 0

----------------------------------------------------------

-- Se dice que este tipo de funciones
-- son polimórficas (o genéricas)
loMismo :: a -> a
loMismo x = x

-- El concepto utilizado es "Polimorfismo parámetrico"
-- y básicamente podemos decir que el tipo de la función
-- está generalizado

loSegundo :: a -> b -> b
loSegundo x y = y

-- El primer parámetro no necesariamente es igual al segundo

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia xs = False

laPrimera :: (a, b) -> a
laPrimera (x, y) = x



-- Easter Eggs

-- Alumno: Lea Tex
-- EG: Star Wars - A New Hope (wallpaper)

-- Alumno: L. Tittarelli
-- EG: Valorant

-- Alumno: Zekiel
-- EG: FIFA 20
