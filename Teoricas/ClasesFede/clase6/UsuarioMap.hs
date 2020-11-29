import Map3

-- Interfaz
-- emptyM :: Map k v
-- assocM :: Eq k => k -> v -> Map k v -> Map k v
-- lookupM :: Eq k => k -> Map k v -> Maybe v
-- deleteM :: Eq k => k -> Map k v -> Map k v
-- keys :: Map k v -> [k]

valores :: Eq k => Map k v -> [v]
valores m = obtenerValores (keys m) m

obtenerValores :: Eq k => [k] -> Map k v -> [v]
obtenerValores [] m     = []
obtenerValores (k:ks) m = 
	valor (lookupM k m) : obtenerValores ks m

-- data Maybe a = Nothing | Just a

-- Parcial cuando es Nothing
valor :: Maybe v -> v
valor Nothing = error "no se obtener un valor"
valor (Just x) = x

claveExiste :: Eq k => k -> Map k v -> Bool
claveExiste k m = esJust (lookupM k m)

esJust (Just x) = True
esJust Nothing  = False

map1 :: Map String Int
map1 =
	assocM "fede" 123 $
	assocM "ale" 456 $
	assocM "cristian" 789 $
	emptyM

map2 :: Map String Int
map2 = assocM "ale" 999 $ map1

numerosDelLoto :: Map Int Bool
numerosDelLoto = 
	assocM 23 True $
	assocM 44 True $
	assocM 55 True $
	assocM 70 True $
	assocM 99 False $
	assocM 77 False $
	assocM 22 False $
	assocM 33 False $
	emptyM

esNumeroGanador :: Int -> Map Int Bool -> Bool
esNumeroGanador n m = 
	esJust (lookupM n m) && valor (lookupM n m)

	-- if esJust (lookupM n m)
	--    then valor (lookupM n m)
	--    else False

--  otra opcion:
-- 	aparece (lookupM n m)

-- aparece :: Maybe Bool -> Bool
-- aparece Nothing  = False
-- aparece (Just b) = b