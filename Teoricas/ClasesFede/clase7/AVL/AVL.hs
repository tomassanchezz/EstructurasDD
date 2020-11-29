{- 
   Por: Fidel
   Para: Orga2
   Fecha: 10-2009
   Status: terminado
   Observaciones:
   * Es un tipo abstracto, por lo que no se puede hacer pattern matching
   * El nombre viene de las iniciales de sus descubridores: Adelson-Velskii y Landis
-}
module AVL
(AVL, emptyAVL , isEmptyAVL , findAVL , insertAVL , deleteAVL, inorderAVL, renderAVL) 
where

import PlusInt
import BinaryTree

-- INTERFASE
-- Tipo: AVL kv

emptyAVL   :: AVL kv
isEmptyAVL :: AVL kv -> Bool
findAVL    :: Ord kv => kv -> AVL kv -> Maybe kv
insertAVL  :: Ord kv => kv -> AVL kv -> AVL kv
deleteAVL  :: Ord kv => kv -> AVL kv -> AVL kv

--show,
renderAVL :: Show kv => AVL kv -> String

-- Auxiliares
heightAVLrep   :: AVLrep kv -> Int
leafAVLrep     :: kv -> AVLrep kv
sJoinAVLrep    :: kv -> AVLrep kv -> AVLrep kv -> AVLrep kv
joinAVLrep     :: kv -> AVLrep kv -> AVLrep kv -> AVLrep kv
lJoinAVLrep    :: kv -> AVLrep kv -> AVLrep kv -> AVLrep kv
rJoinAVLrep    :: kv -> AVLrep kv -> AVLrep kv -> AVLrep kv
splitMaxAVLrep :: AVLrep kv -> (kv, AVLrep kv)

-- IMPLEMENTACION
type AVLrep a = Tree (PlusInt a)
newtype AVL a = AVL (AVLrep a)

 -- INVARIANTE DE REPRESENTACION:
 --  siendo t = NodeT h k v ti td
 --  * t es un BST
 --  * h = height t
 --  * abs (height ti - height td) <= 1 (condición de AVL 1)
 --  * ti y td cumplen las condiciones de AVLs (condicion de AVL 2)

emptyAVL = AVL EmptyT
isEmptyAVL (AVL EmptyT) = True
isEmptyAVL (AVL _)      = False

findAVL kv (AVL t) = find kv t
  where find _  EmptyT                   = Nothing
        find kv (NodeT (PI _ kv') ti td) =
           if kv==kv'
            then Just kv'
            else if kv<kv'
                  then find kv ti
                  else find kv td

insertAVL kv (AVL t) = AVL (insert kv t)
  where insert kv EmptyT = leafAVLrep kv
        insert kv (NodeT (PI _ kv') ti td) = 
            if kv==kv'
             then joinAVLrep kv ti td
             else if kv<kv'
                   then joinAVLrep kv' (insert kv ti) td
                   else joinAVLrep kv' ti (insert kv td)

deleteAVL kv (AVL t) = AVL (delete kv t)
  where delete kv EmptyT = EmptyT
        delete kv (NodeT (PI h kv') ti td) =
            if kv==kv'
             then if isEmptyT ti
                   then td
                   else let (kvi, ti') = splitMaxAVLrep ti
                         in joinAVLrep kvi ti' td
             else if kv<kv'
                   then joinAVLrep kv' (delete kv ti) td
                   else joinAVLrep kv' ti (delete kv td)

inorderAVL (AVL t) = inorder t
  where inorder EmptyT               = []
        inorder (NodeT (PI h kv) ti td) = inorder ti ++ [kv] ++ inorder td

instance (Show kv) => Show (AVL kv) where
  show (AVL t) = show t

renderAVL (AVL t) = renderT t

-- Auxiliares
leafAVLrep kv = leafT (PI 1 kv)

splitMaxAVLrep (NodeT (PI h kv) ti td)    =
   if isEmptyT td
    then (kv, ti)
    else let (kv', td') = splitMaxAVLrep td
          in (kv', joinAVLrep kv ti td')

heightAVLrep EmptyT               = 0
heightAVLrep (NodeT (PI h _) _ _) = h

sJoinAVLrep kv ti td = NodeT (PI (1+max (heightAVLrep ti)
                                        (heightAVLrep td))
                                 kv)
                             ti td

joinAVLrep kv ti td =
  {- PRECOND: 
      * ti y td son BSTs
      * las claves de ti son menores que kv
      * las claves de td son mayores que kv
      * ti y td son AVLs
      * PERO ti y td pueden tener mas altura que lo necesario!!! (pero no deben!)
         (ojo: ti dos mas que td, o td dos mas que ti, pues antes eran AVLs...)
   -}   
   let hi = heightAVLrep ti
       hd = heightAVLrep td
	in if      abs (hi-hd) <= 1
       then    sJoinAVLrep kv ti td
       else if hi == hd + 2
       then    lJoinAVLrep kv ti td
       else if hd == hi + 2
       then    rJoinAVLrep kv ti td
       else -- Otros casos que nunca se alcanzan!!!
            -- con dos joinAVL cada uno (o sea, más caros que O(log n)!)
			   error "Se viola el invariante de representación!"

lJoinAVLrep kv (NodeT (PI hi kvi) tii tid) td =
  -- PRECOND: ti es dos más profundo que td (y por lo tanto es no vacio)
   let hii = heightAVLrep tii
       hid = heightAVLrep tid
    in if hii >= hid
        then sJoinAVLrep kvi tii (sJoinAVLrep kv tid td)
        else let (NodeT (PI _ kvid) tidi tidd) = tid
		      in sJoinAVLrep kvid
                             (sJoinAVLrep kvi tii tidi)
                             (sJoinAVLrep kv  tidd td)
{-
              kv                      kvi
			 /  \                    /   \
			/    \                  /     \
		 kvi     td     ----->    tii     kv
	     / \                             /  \
		/ ti\                           /    \
	  tii   tid                       tid    td
	  
              kv                      kvid
			 /  \                    /    \
			/    \                  /      \
		 kvi     td     ----->   kvi        kv
	     / \                     /  \      /  \
		/ ti\                   /    \    /    \
	  tii  kvid               tii  tidi  tidd  td
           /   \
          / tid \
	    tidi   tidd
-}

rJoinAVLrep kv ti (NodeT (PI hd kvd) tdi tdd) =
  -- PRECOND: td es dos más profundo que ti (y por lo tanto es no vacio)
   let hdi = heightAVLrep tdi
       hdd = heightAVLrep tdd
    in if hdi <= hdd
        then sJoinAVLrep kvd (sJoinAVLrep kv ti tdi) tdd
        else let (NodeT (PI _ kvdi) tdii tdid) = tdi
		      in sJoinAVLrep kvdi
                             (sJoinAVLrep kv ti tdii)
                             (sJoinAVLrep kvd tdid tdd)
						  