-- Resolucion Autores

-- Checksum (Eq, Ord)
-- Persona (Eq, Ord)

data Organizador = MKO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

-- Propósito: Un organizador vacío.
-- Eficiencia: O(1)
nuevo :: Organizador
nuevo = MKO emptyM emptyM

-- Propósito: Agrega al organizador un programa con el Checksum indicado; el 
-- conjunto es el conjunto de personas autores de dicho programa.
-- Precondición: el identificador del programa que se agrega no fue usado
-- previamente en el organizador, y el Set de personas no está vacío.
-- Eficiencia: no hay ninguna garantía de eficiencia.
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MKO mc mp) ch spers = 
    MKO (assocM ch spers mc) (assocM )
