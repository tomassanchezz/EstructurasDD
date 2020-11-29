-- constante O(1)
-- logaritmico O(log n)
-- lineal O(n)
-- cuadratico O(n^2)

data Dir = Izq | Der deriving Eq

data Objeto = Tesoro | Chatarra

data Mapa = Fin
          | Bifurcacion Objeto Mapa Mapa

-- Costo: lineal, O(n)
hayTesoro :: Mapa -> Bool
hayTesoro Fin = False
hayTesoro (Bifurcacion o m1 m2) =
	   esTesoro o
	|| hayTesoro m1
	|| hayTesoro m2

esTesoro Tesoro = True
esTesoro Chatarra = False

-- Costo: logaritmico, O(log n)
-- n es la cantidad de elementos del arbol
-- Prec.: el arbol esta balanceado
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn ds Fin = False
hayTesoroEn [] (Bifurcacion o m1 m2) =
	esTesoro o
hayTesoroEn (d:ds) (Bifurcacion o m1 m2) =
	if d == Izq
	   then hayTesoroEn ds m1
	   else hayTesoroEn ds m2

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- Costo: logaritmico, O(log n)
-- Prec.:
-- 1. el arbol esta completo por nivel
-- de izquierda a derecha
-- 2. el camino que me pasan es al EmptyT
-- a completar para que 1. se siga cumpliendo
insertAt :: a -> [Dir] -> Tree a -> Tree a
insertAt e [] EmptyT = NodeT e EmptyT EmptyT
insertAt e (d:ds) (NodeT x ti td) =
	if d == Izq
	   then NodeT x (insertAt e ds ti) td
	   else NodeT x ti (insertAt e ds td)