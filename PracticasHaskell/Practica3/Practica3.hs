data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

celda :: Celda
celda = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))

nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia       = 0
nroBolitas c (Bolita col cel) =  
    if esColor c col
      then 1 + nroBolitas c (Bolita col cel)
      else nroBolitas c (Bolita col cel)

esColor :: Color -> Color -> Bool
esColor Azul Azul = True
esColor Rojo Rojo = True
esColor _    _    = False

poner :: Color -> Celda -> Celda
poner c (CeldaVacia)     = Bolita c CeldaVacia
poner c (Bolita col cel) = Bolita col (poner c cel)

sacar :: Color -> Celda -> Celda
sacar c (CeldaVacia)     = CeldaVacia
sacar c (Bolita col cel) = 
  if esColor c col
    then cel
    else Bolita col (sacar c cel)

ponerN :: Int -> Color -> Celda -> Celda 
ponerN 0 c CeldaVacia       = CeldaVacia
ponerN n c CeldaVacia       = ponerN (n-1) c (poner c CeldaVacia)
ponerN n c (Bolita col cel) = Bolita col (ponerN n c cel) 

-----------------------------------------------------------------------------
data Objeto = Cacharro | Tesoro
data Camino = Fin 
            | Cofre [Objeto] Camino 
            | Nada Camino

camino :: Camino
camino = Nada 
          (Cofre [Cacharro] 
            (Cofre [Cacharro, Tesoro] 
              Fin))

hayTesoro :: Camino -> Bool
hayTesoro Fin           = False
hayTesoro (Nada c)      = hayTesoro c
hayTesoro (Cofre obs c) = tieneTesoro obs || hayTesoro c

tieneTesoro :: [Objeto] -> Bool
tieneTesoro []     = False
tieneTesoro (x:xs) = esTesoro x || tieneTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Nada c)      = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre obs c) = 
  if tieneTesoro obs 
    then 0
    else 1 + pasosHastaTesoro c

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n Fin           = False
hayTesoroEn 0 (Nada c)      = False
hayTesoroEn 0 (Cofre obs c) = tieneTesoro obs
hayTesoroEn n (Nada c)      = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre obs c) = hayTesoroEn (n-1) c

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n c = cantidadDeTesoros c >= n

-- Considerando que solo puede haber un tesoro adentro de un cofre.
cantidadDeTesoros :: Camino -> Int
cantidadDeTesoros Fin           = 0
cantidadDeTesoros (Nada c)      = cantidadDeTesoros c
cantidadDeTesoros (Cofre obs c) = 
  if tieneTesoro obs
    then 1 + cantidadDeTesoros c
    else cantidadDeTesoros c

cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre n m Fin = 0
cantTesorosEntre 0 m camino = contarTesorosHasta m camino 
cantTesorosEntre n m (Nada c) = cantTesorosEntre (n-1) (m-1) c
cantTesorosEntre n m (Cofre obs c) = cantTesorosEntre (n-1) (m-1) c

contarTesorosHasta :: Int -> Camino -> Int
contarTesorosHasta n Fin = 0
contarTesorosHasta 0 c   = 0
contarTesorosHasta n (Nada c) = contarTesorosHasta (n-1) c
contarTesorosHasta n (Cofre obs c) = cantTesorosCofre obs + contarTesorosHasta (n-1) c

cantTesorosCofre :: [Objeto] -> Int
cantTesorosCofre [] = 0
cantTesorosCofre (x:xs) = 
  if esTesoro x
    then 1 + cantTesorosCofre xs
    else cantTesorosCofre xs
-----------------------------------------------------------------------------
data Tree a = EmptyT 
            | NodeT a (Tree a) (Tree a) deriving Show

sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT x ti td) = x + sumarT ti + sumarT td

sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT x ti td) = 1 + sizeT ti + sizeT td

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT x ti td) = NodeT (x*2) (mapDobleT ti) (mapDobleT td)

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x (EmptyT)        = False
perteneceT x (NodeT a ti td) = x == a || perteneceT x ti || perteneceT x td

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x (EmptyT)       = 0
aparicionesT x (NodeT a ti td) = 
  if x == a
    then 1 + aparicionesT x ti + aparicionesT x td
    else aparicionesT x ti + aparicionesT x td

toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT x ti td) = x : (toList ti ++ toList td)

heightT :: Tree a -> Int
heightT EmptyT                  = 0
heightT (NodeT a EmptyT EmptyT) = 0
heightT (NodeT a t1 t2)         = 1 + max (heightT t1) (heightT t2)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT a t1 t2) = NodeT a t2 t1

leaves :: Tree a -> [a]
leaves EmptyT                  = []
leaves (NodeT a EmptyT EmptyT) = [a] 
leaves (NodeT a t1 t2)         = leaves t1 ++ leaves t2

levelN :: Int -> Tree a -> [a]
levelN n EmptyT          = []
levelN 0 (NodeT a t1 t2) = [a]
levelN n (NodeT a t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2 

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT a t1 t2) = [a] : unirPerLevel (listPerLevel t1) (listPerLevel t2)

unirPerLevel :: [[a]] -> [[a]] -> [[a]]
unirPerLevel [] ys         = ys
unirPerLevel xs []         = xs
unirPerLevel (x:xs) (y:ys) = (x ++ y) : unirPerLevel xs ys

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT a t1 t2) = a : ramaMasLarga (masLargoEntre t1 t2)

masLargoEntre :: Tree a -> Tree a -> Tree a
masLargoEntre EmptyT t = t
masLargoEntre t EmptyT = t
masLargoEntre t1 t2    = 
  if heightT t1 > heightT t2
    then t1
    else t2 

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT a t1 t2) = agregarATodos a (todosLosCaminos t1 ++ todosLosCaminos t2)

agregarATodos :: a -> [[a]] -> [[a]]
agregarATodos x [] = []
agregarATodos x (xs:xss) = (x : xs) : (agregarATodos x xss)
----------------------------------------------------------------------------------
data ExpL = Valor Bool
          | And   ExpL ExpL
          | Or    ExpL ExpL
          | Not   ExpL
          deriving Show

exp1 :: ExpL
exp1 = And (Valor True) (Valor False)

exp2 :: ExpL
exp2 = Or exp1 (Valor True)

evalL :: ExpL -> Bool
evalL (Valor b)   = b
evalL (And e1 e2) = evalL e1 && evalL e2
evalL (Or e1 e2)  = evalL e1 || evalL e2
evalL (Not e1)    = not (evalL e1)

simplificarL :: ExpL -> ExpL
simplificarL (And e1 e2) = simplAnd e1 e2
simplificarL (Or e1 e2)  = simplOr  e1 e2
simplificarL (Not e1)    = simplNot e1 
simplificarL x           = x 

simplAnd :: ExpL -> ExpL -> ExpL
simplAnd (Valor True) x  = x
simplAnd x (Valor True)  = x
simplAnd (Valor False) x = Valor False
simplAnd x (Valor False) = Valor False
simplAnd x y             = And x y

simplOr :: ExpL -> ExpL -> ExpL
simplOr (Valor True) x  = Valor True
simplOr x (Valor True)  = Valor True
simplOr (Valor False) x = x
simplOr x (Valor False) = x
simplOr x y             = Or x y

simplNot :: ExpL -> ExpL
simplNot (Not e1) = e1
simplNot x        = x

esNot :: ExpL -> Bool
esNot (Not e1) = True
esNot _        = False

t1 :: Tree Int
t1 = NodeT 32
        (NodeT 35
          (NodeT 100
              (NodeT 15
                  EmptyT
                  EmptyT)
              EmptyT)
          EmptyT)
        (NodeT 40
          (NodeT 50
              EmptyT
              EmptyT)
          EmptyT)

t2 :: Tree Int
t2 = NodeT 1 (NodeT 2 (NodeT 9 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)) (NodeT 5 EmptyT EmptyT)

pad :: [[Char]] -> [[Char]]
pad = zipWith (++) (repeat "   ")

arreglarT :: Show a => Tree a -> IO()
arreglarT EmptyT = putStrLn "-"

arreglarT tree = printRecursive (prettyprint_helper tree)
  where 
    printRecursive [] = putStrLn ""
    printRecursive (x:xs) = do
      putStrLn x
      printRecursive xs

prettyprint_helper (NodeT x left right)
  = (show x) : (prettyprint_subtree left right)
    where
      prettyprint_subtree left right =
        ((pad "+- " "|  ") (prettyprint_helper right))
        ++ ((pad "`- " "   ") (prettyprint_helper left))
      pad first rest = zipWith (++) (first : repeat rest)

prettyprint_helper EmptyT = [""]