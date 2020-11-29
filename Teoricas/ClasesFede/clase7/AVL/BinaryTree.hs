{- 
   Por: Fidel
   Para: Orga2
   Fecha: 10-2009
   Status: terminado
   Observaciones:
   * Es una estructura algebraica, y por lo tanto, es posible hacer pattern matching sobre Tree!
-}
module BinaryTree
(Tree(..), isEmptyT, leafT, renderT) 
where

-- INTERFASE
{-
   Tipo: Tree a

   -- Constructores
   EmptyT :: Tree a
   NodeT :: a -> Tree a -> Tree a -> Tree a
   -- Inspectores
   isEmptyT :: Tree a -> Bool
   root :: Tree a -> a               -- Parcial en EmptyT
   left, right :: Tree a -> Tree a   -- Parcial en EmptyT

   -- Operaciones de dibujjo
   show, renderT :: Show a => Tree a -> String
-}

{- -- Derivadas
   leafT :: a -> Tree a
-}

-- Operaciones derivadas
leafT x = NodeT x EmptyT EmptyT

-- IMPLEMENTACION
data Tree a = EmptyT 
            | NodeT { root  :: a
                    , left  :: Tree a
                    , right :: Tree a
                    }

isEmptyT EmptyT = True
isEmptyT _      = False

-------------------------------------------
-- Operaciones de impresion de arboles
-------------------------------------------
instance (Show a) => Show (Tree a) where
  show t = showsTree "\n" "\n" t ""
    where showsTree ctxtI ctxt EmptyT          = showString ctxtI . showString "E" 
          showsTree ctxtI ctxt (NodeT x ti td) = showString ctxtI . showString "N " . shows x 
                                                                  . showsTree (ctxt ++ "+ ") (ctxt ++ "| ") ti 
                                                                  . showsTree (ctxt ++ "+ ") (ctxt ++ "  ") td
		  
renderT t = foldr (\line screen -> line ++ "\n" ++ screen) "" (renderT' [] t)
  where renderT' xs EmptyT              = renderSpine 0 (ocupaDeAnchoXs xs) (reverse xs) 0
        renderT' xs (NodeT x ti EmptyT) = renderT' (x:xs) ti
        renderT' xs (NodeT x EmptyT td) = renderT' (x:xs) td
        renderT' xs (NodeT x ti td)     = renderSpine (ocupaDeAnchoT ti) (ocupaDeAnchoXs (x:xs))
                                                      (reverse (x:xs)) (ocupaDeAnchoT td)
                                          ++ combinar (ocupaDeAnchoT ti) (renderT' [] ti) 
                                                      (ocupaDeAnchoXs (x:xs)) 
                                                      (renderT' [] td) (ocupaDeAnchoT td)

        combinar li []     lx ys     ld = map (\y -> nBlancos li ++ nBlancos lx ++ y) ys
        combinar li xs     lx []     ld = map (\x -> x ++ nBlancos lx ++ nBlancos ld) xs
        combinar li (x:xs) lx (y:ys) ld = (x ++ nBlancos lx ++ y) : combinar li xs lx ys ld

        renderSpine li lx xs ld = map (\x -> renderLine li lx x ld) xs

        renderLine li lx x ld = nBlancos li ++ nBlancos (lx - ocupaDeAnchoX x) ++ show x ++ nBlancos ld

        nBlancos n = [' ' | i <- [1..n]]

        ocupaDeAnchoT EmptyT                  = 0
        ocupaDeAnchoT (NodeT x EmptyT EmptyT) = ocupaDeAnchoX x
        ocupaDeAnchoT (NodeT x ti     EmptyT) = max (ocupaDeAnchoX x) (ocupaDeAnchoT ti)
        ocupaDeAnchoT (NodeT x EmptyT td)     = max (ocupaDeAnchoX x) (ocupaDeAnchoT td)
        ocupaDeAnchoT (NodeT x ti     td)     = ocupaDeAnchoT ti + ocupaDeAnchoX x + ocupaDeAnchoT td

        ocupaDeAnchoXs xs = maximum (map ocupaDeAnchoX xs)
        ocupaDeAnchoX x = length (show x)