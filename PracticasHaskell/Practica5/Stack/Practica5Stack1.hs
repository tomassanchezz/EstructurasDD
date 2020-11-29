-- Practica 5 Stack
module Practica5Stack1 (
	Stack,
	emptyS,
	isEmptyS,
	push,
	top,
	pop,
	lenS
) where

data Stack a = Stack [a] deriving Show
--Invariantes:
-- No tiene.

-- Costo = Constante
emptyS :: Stack a
emptyS = Stack []

-- Costo = Constante
isEmptyS :: Stack a -> Bool
isEmptyS (Stack xs) = null xs 

-- Costo = Constante
push :: a -> Stack a -> Stack a
push a (Stack xs) = Stack (a:xs)

-- Costo = Constante
top :: Stack a -> a
top (Stack xs) = head xs 

-- Costo = Constante
pop :: Stack a -> Stack a
pop (Stack xs) = Stack (tail xs)

-- Costo = Constante
lenS :: Stack a -> Int
lenS (Stack []) = 0
lenS (Stack xs) = length xs
