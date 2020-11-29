-- Resolucion Entreprise

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

-- Cada tripulante puede estar en un sector como máximo.

-- Se guarda al sector con más tripulantes de la nave y 
-- cuántos tripulantes tiene ese sector.

-- Los tripulantes se ordenan por rango de mayor a menor en la Heap

-- (no se confunda, findMin devuelve al tripulante con mayor rango).


-- Propósito: Crea una nave con todos esos sectores sin tripulantes.
-- Precondición: la lista de sectores no está vacía
-- Costo: O(S log S) siendo S la cantidad de sectores de la lista.
naveVacia :: [Sector] -> Nave
