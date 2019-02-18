module Practica1p2 where

{- |1| Función permutaciones: Recibe una lista de elementos. Regresa todas las   
   permutaciones de los elementos de la lista.
   Hint: Se recomienda el uso de la función intercala.
-}
permutaciones:: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x:xs) = 
    [ys | zs <- permutaciones xs, ys <- intercala x zs]

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

-- |4| Función ternasPitagoricas: Recibe un número y regresa la lista de ternas pitagóricas que correspondan.
ternasPitagoricas:: Int -> [(Int,Int,Int)]
ternasPitagoricas = error "Implementar"

{- |5| Función isSubSet: Recibe dos listas. Nos dice si la primera lista está
   contenida en la segunda lista.
-} 
isSubSet :: Eq a => [a] -> [a] -> Bool
isSubSet = error "Implementar"

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Ord, Eq)

leaf x = Branch x Empty Empty

-- |6| Función deleteT: Elimina el elemento de un árbol binario. 
deleteT :: (Eq a, Ord a) => a -> Tree a -> Tree a
deleteT = error "Implementar"

-- |7| Función balanced: Nos dice si un árbol binario esta balanceado.
balanced :: Tree a -> Bool
balanced = error "Implementar"

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
