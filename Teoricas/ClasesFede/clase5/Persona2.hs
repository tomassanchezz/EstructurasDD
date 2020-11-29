module Persona2(
	Persona,
	crearPersona,
	edad,
	nombre,
	apellido,
	nombreCompleto) where

-- Representación
data Persona = ConsP Int String String deriving Show
-- Invariante de Representación
-- Si ConsP e n a es una Persona, entonces:
-- 1. e >= 0
-- 2. n no es un string vacio 
-- 3. a no es un string vacio

crearPersona :: Int -> String -> String -> Persona
crearPersona e n a =
	if e < 0 || n == "" || a == ""
	   then error "edad, nombre o apellido vacio"
	   else ConsP e n a

edad :: Persona -> Int
edad (ConsP e n a) = e

nombre :: Persona -> String
nombre (ConsP e n a) = n

apellido :: Persona -> String
apellido (ConsP e n a) = a

nombreCompleto :: Persona -> String
nombreCompleto (ConsP e n a) = n ++ " " ++ a