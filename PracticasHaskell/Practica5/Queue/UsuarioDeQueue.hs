--Practica 5 Usuario Queue
import Practica5Queue1

-- emptyQ :: Queue a
-- isEmptyQ :: Queue a -> Bool
-- queue :: a -> Queue a -> Queue a
-- firstQ :: Queue a -> a
-- dequeue :: Queue a -> Queue a

lengthQ :: Queue a -> Int
lengthQ qe = 
	if isEmptyQ qe
		then 0
		else 1 + lengthQ (dequeue qe)

queueToList :: Queue a -> [a]
queueToList qe = 
	if isEmptyQ qe
		then []
		else firstQ qe : queueToList (dequeue qe)

unionQ :: Queue a -> Queue a -> Queue a
unionQ qe1 qe2 = 
	if isEmptyQ qe2 
		then qe1
		else unionQ (queue (firstQ qe2) qe1) (dequeue qe2)
