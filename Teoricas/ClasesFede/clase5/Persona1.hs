module Persona1(
	Persona,
	crearPersona,
	edad,
	nombre,
	apellido,
	nombreCompleto) where

-- Representación
data Persona = ConsP Int String deriving Show
-- Invariante de Representación
-- Si ConsP e n es una Persona, entonces:
-- 1. e >= 0
-- 2. n un string con una parte inicial no vacia, un espacio
-- en blanco en el medio, y otra parte no vacia luego

crearPersona :: Int -> String -> String -> Persona
crearPersona e n a =
	if e < 0 || n == "" || a == ""
	   then error "edad, nombre o apellido vacio"
	   else ConsP e (n ++ " " ++ a)

edad :: Persona -> Int
edad (ConsP e n) = e

nombre :: Persona -> String
nombre (ConsP e n) = obtenerNombre n

apellido :: Persona -> String
apellido (ConsP e n) = obtenerApellido n

nombreCompleto :: Persona -> String
nombreCompleto (ConsP e n) = n

obtenerNombre :: String -> String
obtenerNombre [] = error "no puede tener nombre vacio"
obtenerNombre (x:xs) =
	if x == ' '
	   then []
	   else x : obtenerNombre xs

obtenerApellido :: String -> String
obtenerApellido [] = error "no puede tener nombre vacio"
obtenerApellido (x:xs) =
	if x == ' '
	   then xs
	   else obtenerApellido xs