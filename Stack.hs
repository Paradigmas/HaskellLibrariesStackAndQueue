-- Stack library
module Stack 
    (Stack,
    empty,
    size,
    top,
    push,
    pop,
    find,
    sumSt,
    meanSt,
    maxSt,
    minSt,
    stackInList,
    listInStack)
     where

--Stack definition
data Stack a = Empty | St [a]
    deriving (Eq, Show)

--Emptying the stack
empty _ = Empty

--Stack size
size Empty = 0
size (St []) = 0
size (St (a:b)) = 1 + (size (St b))

--Insertion of the element in the Stack
push element (St stack) = St(element:stack)
push element Empty = (St [element])

--The top element in the stack
top Empty = error "Empty Stack"
top (St []) = error "Empty Stack"
top (St (a:b)) = a

--Deleting element in the stack
pop Empty = error "Empty Stack"
pop (St []) = error "Empty Stack"
pop (St (a:b)) | size (St (a:b))==1 = Empty
               | otherwise = St b

--Find a element in the stack
find element Empty = False
find element (St []) = False
find element (St (a:b))
        |a == element = True
        | otherwise = (find element (St b))

--The sum of the elements of the stack
sumSt Empty = 0
sumSt (St []) = 0
sumSt (St (a:b)) = a + (sumSt (St b))

--The mean of the elements of the stack
meanSt Empty = 0
meanSt (St []) = 0
meanSt (St a) = (sumSt (St a))/ (size (St a))

--The max element of the stack
maxSt Empty = error "Empty Stack"
maxSt (St []) = error "Empty Stack"
maxSt (St (a:b))
        |size(St(a:b)) == 1 = a
        | otherwise = max a (maxSt (St b))

--The min element of the stack
minSt Empty = error "Empty Stack"
minSt (St []) = error "Empty Stack"
minSt (St (a:b))
        |size(St(a:b)) == 1 = a
        | otherwise = min a (minSt (St b))

--Transformation a stack in a list
stackInList Empty = []
stackInList (St x) = x

--Transformation a list in a stakc
listInStack x = St x
