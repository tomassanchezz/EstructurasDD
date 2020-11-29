-- Evaluacion Parcial
-- 1) --------------------------------------------------------------------------------------------------------------------
data Sanguche = Pan Relleno deriving Show

data Relleno = Feta TipoDeFeta Relleno
			 			 | Aire
			 			 deriving Show

data TipoDeFeta = Queso
				        | Jamon
				   			| Mortadela
								| Salame
								deriving Show

sanguche1 :: Sanguche
sanguche1 = Pan (Feta Queso (Feta Jamon (Feta Jamon (Feta Jamon (Feta Jamon (Feta Queso (Feta Queso (Feta Queso Aire))))))))

sanguche2 :: Sanguche
sanguche2 = Pan (Aire)

--a)
--Proposito: Dado un sanguche, indica si el relleno es solo de aire.
rellenoDeAire :: Sanguche -> Bool
rellenoDeAire (Pan r) = esAire r

esAire :: Relleno -> Bool
esAire Aire = True
esAire _   	= False 

--b)
--Propósito: Dado un sanguche indica si solo tiene fetas de jamon.
esTortitaDeJamon :: Sanguche -> Bool
esTortitaDeJamon (Pan r) = esTortitaDeJamonR r

esTortitaDeJamonR :: Relleno -> Bool
esTortitaDeJamonR Aire = True
esTortitaDeJamonR (Feta t r) = esJamon t && esTortitaDeJamonR r

esJamon :: TipoDeFeta -> Bool
esJamon Jamon = True
esJamon _	    = False

--c)
--Propósito: Dados un número n y un tipo de feta, agrega n fetas de ese tipo, 
--			 al principio del relleno del sanguche dado.
mandaleNDe :: Int -> TipoDeFeta -> Sanguche -> Sanguche
mandaleNDe 0 t s 	     = s
mandaleNDe n t (Pan r) = Pan (agregarNDe n t r)

agregarNDe :: Int -> TipoDeFeta -> Relleno -> Relleno
agregarNDe 0 t r = r
agregarNDe n t r = Feta t (agregarNDe (n-1) t r)

--d)
--Propósito: Quita todo el queso del relleno al sanguche dado.
peroSinQueso :: Sanguche -> Sanguche
peroSinQueso (Pan r) = Pan (rellenoSinQueso r)

--Hice dos implementaciones porque no estaba seguro si la primera es del todo correcta aunque ande.

-- rellenoSinQueso :: Relleno -> Relleno
-- rellenoSinQueso Aire = Aire
-- rellenoSinQueso (Feta Queso r) = rellenoSinQueso r
-- rellenoSinQueso (Feta t r)     = Feta t (rellenoSinQueso r)

rellenoSinQueso :: Relleno -> Relleno
rellenoSinQueso Aire = Aire
rellenoSinQueso (Feta t r) = 
	if esQueso t
		then rellenoSinQueso r
		else Feta t (rellenoSinQueso r)

esQueso :: TipoDeFeta -> Bool
esQueso Queso = True
esQueso _			= False

--e)
--Propósito: Devuelve una lista de fetas del relleno del sanguche, 
-- 					 junto con su cantidad de apariciones.

ordenadosPorCantidad :: Sanguche -> [(TipoDeFeta, Int)]
ordenadosPorCantidad (Pan r) = cantidadesDeRelleno r

cantidadesDeRelleno :: Relleno -> [(TipoDeFeta, Int)]
cantidadesDeRelleno Aire = []
cantidadesDeRelleno (Feta t r) = 
	sumarTipo t (cantidadesDeRelleno r)

sumarTipo :: TipoDeFeta -> [(TipoDeFeta, Int)] -> [(TipoDeFeta, Int)]
sumarTipo t [] 		 = [(t, 1)]
sumarTipo t (x:xs) = 
	if tiposIguales t (fst x)
		then (t, 1 + snd x) : xs
		else x : sumarTipo t xs

tiposIguales :: TipoDeFeta -> TipoDeFeta -> Bool
tiposIguales Queso Queso 				 = True
tiposIguales Mortadela Mortadela = True
tiposIguales Jamon Jamon 				 = True
tiposIguales Salame Salame 			 = True
tiposIguales _ _								 = False
--------------------------------------------------------------------------------------------------------------------------
-- 2) --------------------------------------------------------------------------------------------------------------------
-- Las llaves son simples numeros.
type Llave = Int

-- Existen direcciones que indican caminos dentro del laberinto.
data Dir = Izq | Der deriving Show

-- Los cofres tienen una cantidad de oro,
-- pero solo accesible para aquellos que posean las llaves que lo abren.
-- Pueden existir cofres que no requieran de llaves.
data Cofre = Cofre [Llave] Int

-- Un laberinto posee salidas, celdas y bifurcaciones.
-- En las celdas hay cofres, en las salidas no hay nada,
-- y en las bifurcaciones puedo ir hacia uno u otro lado.
data Laberinto = Salida
							 | Celda Cofre
							 | Bifurcacion Laberinto Laberinto

ll1 :: Llave
ll1 = 1

ll2 :: Llave
ll2 = 2

ll3 :: Llave
ll3 = 3

ll4 :: Llave
ll4 = 4

ll5 :: Llave
ll5 = 5

ll6 :: Llave
ll6 = 6

cofre1 :: Cofre
cofre1 = Cofre [ll1, ll2] 10

cofre2 :: Cofre
cofre2 = Cofre [ll2, ll3, ll4] 20

cofre3 :: Cofre
cofre3 = Cofre [ll5, ll6] 30

lab1 :: Laberinto
lab1 = Bifurcacion (Celda cofre1) 
									 (Bifurcacion (Celda cofre2) 
									 							(Bifurcacion (Celda cofre3) 
									 													 (Bifurcacion (Celda cofre3) Salida)))
--a)
--Propósito: Indica cuantas salidas posee un laberinto.
cantidadDeSalidas :: Laberinto -> Int
cantidadDeSalidas Salida 				= 1
cantidadDeSalidas (Celda c) = 0
cantidadDeSalidas (Bifurcacion l1 l2) = 
	cantidadDeSalidas l1 + cantidadDeSalidas l2

--b)
--Propósito: Dado un laberinto indica qué llaves se deben tener para poder abrir todos sus cofres.
--Nota: el resultado no debe tener llaves repetidas.
queLlavesDeboTener :: Laberinto -> [Llave]
queLlavesDeboTener Salida 			 = []
queLlavesDeboTener (Celda c) = llavesDeCofre c
queLlavesDeboTener (Bifurcacion l1 l2) = sinRepetidos (queLlavesDeboTener l1 ++ queLlavesDeboTener l2)

llavesDeCofre :: Cofre -> [Llave]
llavesDeCofre (Cofre lls o) = lls

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = 
	if pertenece x xs
		then sinRepetidos xs
		else x : sinRepetidos xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = x == a || pertenece a xs

--c)
--Propósito: Indica cuánto oro puede conseguirse dada una lista de llaves.
cantidadDeOroCon :: [Llave] -> Laberinto -> Int
cantidadDeOroCon [] l 			 	 = 0
cantidadDeOroCon lls Salida 	 = 0
cantidadDeOroCon lls (Celda c) = 
	if puedoAbrir lls c 
		then cantOro c
		else 0
cantidadDeOroCon lls (Bifurcacion l1 l2) = 
	cantidadDeOroCon lls l1 + cantidadDeOroCon lls l2

puedoAbrir :: [Llave] -> Cofre -> Bool
puedoAbrir [] c = False
puedoAbrir (x:xs) (Cofre lls oro) = pertenece x lls || puedoAbrir xs (Cofre lls oro)

cantOro :: Cofre -> Int
cantOro (Cofre lls o) = o 

--d)
--Propósito: Dada una lista de direcciones indica si llevan a una posible salida del laberinto.
haySalidaPor :: [Dir] -> Laberinto -> Bool
haySalidaPor [] Salida 		= True
haySalidaPor ds Salida    = True -- Este caso me resulta medio extranio pero por cuestiones de pm lo dejo
haySalidaPor [] l      		= False
haySalidaPor ds (Celda c) = False
haySalidaPor (d:ds) (Bifurcacion l1 l2) = 
	if esIzq d
		then haySalidaPor ds l1
		else haySalidaPor ds l2 

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _		= False

--e)
--Propósito: Indica el camino a la salida más cercana.
--Precondición: Existe al menos una salida
salidaMasCercana :: Laberinto -> [Dir]
salidaMasCercana Salida = []
salidaMasCercana (Bifurcacion l1 l2) = 
	if existeSalida l1 && esMasCercana l1 l2
		then Izq : salidaMasCercana l1
		else Der : salidaMasCercana l2

existeSalida :: Laberinto -> Bool
existeSalida l = cantidadDeSalidas l > 0

esMasCercana :: Laberinto -> Laberinto -> Bool
esMasCercana l1 l2 = pasosALaSalida l1 < pasosALaSalida l2

pasosALaSalida :: Laberinto -> Int
pasosALaSalida Salida = 0
pasosALaSalida (Celda c) = 0
pasosALaSalida (Bifurcacion l1 l2) = 1 + min (pasosALaSalida l1) (pasosALaSalida l2)