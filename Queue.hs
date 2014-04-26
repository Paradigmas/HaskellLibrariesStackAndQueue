-- Queue library
module Queue 
    (Queue,
    empty,
    size,
    push,
    pop,
    front,
    back,
    find,
    sumQu,
    meanQu,
    maxQu,
    minQu,
    queueInList,
    listInQueue)
    where

--Queue definition
data Queue a = Empty | Qu [a]
    deriving (Eq, Show)

--Emptying the queue
empty _ = Empty

--Queue size
size Empty = 0
size (Qu[]) = 0
size (Qu(a:b)) = 1 +(size(Qu b))

--Insertion of the element in the Queue
push element (Qu a) = (Qu (a ++[element]))

--Deleting element in the queue
pop Empty = error "Empty Queue"
pop (Qu[]) = error "Empty Queue"
pop (Qu(a:b)) = Qu b

--The first element in the queue
front Empty = error "Empty Queue"
front (Qu[]) = error "Empty Queue"
front (Qu(a:b)) = a

--The last element in the queue
back Empty = error "Empty Queue"
back (Qu[]) = error "Empty Queue"
back (Qu a) = last a

--Find a element in the queue 
find element Empty = False
find element (Qu []) = False
find element (Qu (a:b))
        |a == element = True
        | otherwise = (find element (Qu b))

--The sum of the elements of the queue
sumQu Empty = 0
sumQu (Qu []) = 0
sumQu (Qu (a:b)) = a + (sumQu (Qu b))

--The mean of the elements of the queue
meanQu Empty = 0
meanQu (Qu []) = 0
meanQu (Qu a) = (sumQu (Qu a))/ (size (Qu a))

--The max element of the queue
maxQu Empty = error "Empty Queue"
maxQu (Qu []) = error "Empty Queue"
maxQu (Qu (a:b))
        |size(Qu(a:b)) == 1 = a
        | otherwise = max a (maxQu (Qu b))

----The min element of the queue
minQu Empty = error "Empty Queue"
minQu (Qu []) = error "Empty Queue"
minQu (Qu (a:b))
        |size(Qu(a:b)) == 1 = a
        | otherwise = min a (minQu (Qu b))

--Transformation a queue in a list
queueInList Empty = []
queueInList (Qu a) = a

--Transformation a list in a queue
listInQueue a = Qu a
