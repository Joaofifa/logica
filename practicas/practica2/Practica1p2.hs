module Practica1p2 where

{- |1| Función permutaciones: Recibe una lista de elementos. Regresa todas las   
   permutaciones de los elementos de la lista.
   Hint: Se recomienda el uso de la función intercala.
-}
permutaciones:: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x:xs) = [ys | zs <- permutaciones xs, ys <- intercala x zs]

{- |2| Función factores: Recibe un número entero positivo n. Regresa la lista
   de sus factores.
-}
factores :: Int -> [Int]
factores n =
    if n < 0 then error "El número debe ser un entero positivo."
    else [f | f <- [1..n], mod n f == 0]

{- |3| Función perfectos: Recibe un número entero positivo n. Regresa la lista
   de números perfectos que se encuentran hasta el número n.
-}
perfectos :: Int -> [Int]
perfectos n = 
    if n < 0 then error "El número debe ser un entero positivo."
    else [p | p <- [1..n], sum (initL (divisores p)) == p]
        where divisores p = [d | d <- [1..n-1], mod p d == 0]

{- |4| Función ternasPitagoricas: Recibe un número entero positivo n. Regresa 
   la lista de ternas pitagóricas que corresponden. 
-}
ternasPitagoricas:: Int -> [(Int,Int,Int)]
ternasPitagoricas n =
    if n < 0 then error "El número debe ser un entero positivo."
    else [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], terna a b c]
        where terna a b c = c*c == a*a + b*b 

-- |5| Función isSubSet: Recibe dos listas y nos dice si la primera lista es subconjunto de la segunda. 
isSubSet :: Eq a => [a] -> [a] -> Bool
isSubSet [] _ = True
isSubSet [a] [] = False
isSubSet [a] (x:xs) = if(a == x)
	 then True
	 else isSubSet [a] xs
isSubSet (y:ys) xs = ((isSubSet [y] xs) && isSubSet ys xs)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Ord, Eq)

leaf x = Branch x Empty Empty

-- |6| Función deleteT: Elimina el elemento de un árbol binario. 
deleteT :: (Eq a, Ord a) => a -> Tree a -> Tree a
deleteT _ Empty = Empty
deleteT a (Branch e l r)= if(a == e)
	then deleteActual (Branch e l r)
	else if (a < e)
		then (Branch e (deleteT a l) r)
		else (Branch e l (deleteT a r))


-- |7| Función balanced: Nos dice si un árbol binario esta balanceado.
balanced :: Tree a -> Bool
balanced Empty = True
balanced (Branch e l r) = if(not(((altura l) - (altura r) > 1) || ((altura l) - (altura r) < -1))) 
	then (balanced l) && (balanced r)
	else False


{- |8| Función pre: Recibe un árbol binario. Regresa la lista con los elementos
   del árbol al ser recorrido en pre-órden. 
-}
pre :: Tree a -> [a]
pre Empty = []
pre (Branch e i d) = e : (pre i ++ pre d)

-- Funciones auxiliares.

{- |Aux. 1| Función intercala: Recibe un elemento y una lista de elementos. 
   Devuelve la lista de listas con el elemento intercalado en una lista.
-}
intercala :: a -> [a] -> [[a]]
intercala a [] = [[a]]
intercala a (x:xs) = (a:x:xs) : [(x:xs) | xs <- intercala a xs]

-- |Aux. 2| Función initL: Regresa la lista sin el último elemento.
initL :: [a] -> [a]
initL [] = error "La lista está vacía."
initL (x:xs) = if (isEmpty xs) then [] else x:(initL xs)

-- |Aux. 3| isEmpty: Nos dice si una lista es la lista vacía.
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

-- |Aux. 4| altura: Nos regresa la altura del nodo actual
altura :: Tree a -> Int
altura Empty = 0
altura (Branch e Empty Empty) = 1
altura (Branch e l r) = if(altura l <= altura r)
	then 1 + altura r
	else 1 + altura l

-- |Aux. 5| deleteActual: Elimina el nodo principal del arbol, y lo cambia por el nodo menor de la rama derecha
deleteActual :: (Eq a, Ord a) => Tree a -> Tree a
deleteActual (Branch e Empty Empty) = Empty
deleteActual (Branch e l Empty) = l
deleteActual (Branch e Empty r) = r
deleteActual (Branch e l r) = if(leftT r)
	then (Branch (leftE r) l (removeLeft 1 r))
	else (Branch (elementT r) l (rightReplace r))

-- |Aux. 6| leftE: Regresa el elemento menor del arbol
leftE :: (Eq a, Ord a) => Tree a ->  a
leftE (Branch e Empty _) = e
leftE (Branch _ l _) = leftE l

-- |Aux. 7| removeLeft: Elimina el elemento menor del arbol
removeLeft :: (Eq a, Ord a) => Int -> Tree a -> Tree a
removeLeft 1 (Branch e Empty Empty) = Empty
removeLeft 1 (Branch e l r) = (Branch e (removeLeft 1 l) r)
removeLeft 0 (Branch e l r) = (Branch e l r)

-- |Aux. 8| leftT: Avisa si el Branch izquierdo esta vacio
leftT :: (Eq a, Ord a) => Tree a -> Bool
leftT (Branch a l r) = if(l == Empty) 
	then False
	else True

-- |Aux. 9| elementT: Regresa el nodo principal del arbol
elementT :: (Eq a, Ord a) => Tree a -> a
elementT (Branch e _ _) = e

-- |Aux. 10| rightReplace: Reemplaza el arbol con la rama derecha
rightReplace :: (Eq a, Ord a) => Tree a -> Tree a
rightReplace (Branch a _ r) = r
