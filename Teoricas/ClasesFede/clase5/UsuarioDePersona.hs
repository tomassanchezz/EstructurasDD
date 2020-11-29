import Persona2
import Data.Char

-- toUpper :: Char -> Char

pasarAMayuscula :: String -> String
pasarAMayuscula [] = []
pasarAMayuscula (x:xs) = toUpper x : pasarAMayuscula xs

-- crearPersona :: Int -> String -> String -> Persona
-- edad :: Persona -> Int
-- nombre :: Persona -> String
-- apellido :: Persona -> String

personaAMayuscula :: Persona -> Persona
personaAMayuscula pers =
	crearPersona (edad pers)
	             (pasarAMayuscula (nombre pers))
	             (pasarAMayuscula (apellido pers))

pers1 = crearPersona 32 "Carlos" "Mujica"
pers2 = crearPersona 44 "Jorge" "Asis"
pers3 = crearPersona 18 "Justin" "Timberlake"
pers4 = crearPersona 18 "Jose" "Maria Listorti"
