--Practica 5 Tipos Abstractos de Datos.
-- Costo = Constante
head :: [a] -> a
head (x:xs) = x

-- Costo = Constante
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-- Costo = Lineal
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Costo = Constante
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Costo = Cuadratica 
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

-- Costo = Lineal
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- Costo = Cuadratica
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
	if pertenece x xs
		then sinRepetidos xs
		else x : sinRepetidos xs

-- equivalente a (++)
-- Costo = Lineal 
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- Costo = Cuadratica 
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

-- Costo = Lineal
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

-- Costo = Lineal
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

-- Costo = Lineal
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

-- Costo = Lineal
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

-- Costo = Lineal
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
	if n == x
		then xs
		else x : sacar n xs

-- Costo = Cuadratica
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
let m = minimo xs
	in m : ordenar (sacar m xs)