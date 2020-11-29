import Termometro3

-- nuevoT :: Termometro
-- estaVacio :: Termometro -> Bool
-- agregarTemperatura :: Int -> Termometro -> Termometro
-- sacarUltimaTemperatura :: Termometro -> Termometro
-- ultimaTemperatura :: Termometro -> Int
-- maxTemperatura :: Termometro -> Int

-- Costo 1 es con Termometro 1
-- Costo 2 es con Termometro 2
-- Costo 3 es con Termometro 3

-- Costo 1: lineal
-- Costo 2: lineal
-- Costo 2: lineal
agregarTemps :: [Int] -> Termometro -> Termometro
agregarTemps []     term = term
agregarTemps (t:ts) term =
	agregarTemperatura t (agregarTemps ts term)

-- Costo 1: lineal
-- Costo 2: cuadratica
-- Costo 3: lineal
temperaturas :: Termometro -> [Int]
temperaturas term =
	if estaVacio term
	   then []
	   else ultimaTemperatura term :
	        temperaturas (sacarUltimaTemperatura term)

term1 = nuevoT
term2 = agregarTemps [10, 30, 40, 50] nuevoT
term3 = agregarTemps [-1, 30, -5, 10] nuevoT

-- Costo 1: lineal
-- Costo 2: cuadratica
-- Costo 3: lineal
cuantasNegativas :: Termometro -> Int
cuantasNegativas term =
	if estaVacio term
	   then 0
	   else if ultimaTemperatura term < 0
               then 1 + cuantasNegativas (sacarUltimaTemperatura term)
               else cuantasNegativas (sacarUltimaTemperatura term)