-- Desventajas de Tipos Algebraicos

-- data Color = Azul | Verde | Rojo | Negro | Amarillo

-- primeraLetra :: Color -> Char
-- primeraLetra Azul = 'A'
-- primeraLetra Verde = 'V'
-- primeraLetra Rojo = 'R'
-- primeraLetra Negro = 'N'

-- ...

-- 1) Si se agregan casos debo ir a todas las funciones
-- que los analizan

data Persona1 = ConsP1 Int String

nombreCompleto :: Persona -> String
nombreCompleto (ConsP e n) = n

pers11 = ConsP1 32 "Carlos Mujica"
pers12 = ConsP1 44 "Jorge Asis"
pers13 = ConsP1 18 "Justin Timberlake"
pers14 = ConsP1 18 "Jose Maria Listorti"

data Persona2 = ConsP2 Int String String

nombreCompleto :: Persona -> String
nombreCompleto (ConsP e n a) = n ++ " " ++ a

pers21 = ConsP2 32 "Carlos" "Mujica"
pers22 = ConsP2 44 "Jorge" "Asis"
pers23 = ConsP2 18 "Justin" "Timberlake"
pers24 = ConsP2 18 "Jose" "Maria Listorti"

-- 2) Si cambio la estructuras debo modificar todo el código
-- que extrae información sobre la misma

pers25 = ConsP2 (-1) "Robert" "Pattison"

-- 3) No puedo forzar al usuario a respetar propiedades

-- 4) No puedo ocultar cómo está definido el tipo de datos
