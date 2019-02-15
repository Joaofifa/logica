module Practica1p2 where

{-|1|
   Función permutaciones: Recibe una lista de elementos y devuelve todas las permutaciones de los elementos de la lista.
   Hint: Se recomienda el uso de la función intercala.
-}
permutaciones:: [a] -> [[a]]
permutaciones = error "Implementar"

-- |2| Función factores: Recibe un entero y regresa la lista de sus factores.
factores :: Int -> [Int]
factores = error "Implementar"

-- |3| Función perfectos: Recibe un número n y devuelve la lista de números perfectos que se encuentran hasta el número n. 
perfectos :: Int -> [Int]
perfectos = error "Implementar"

-- |4| Función ternasPitagoricas: Recibe un número y regresa la lista de ternas pitagóricas que correspondan.
ternasPitagoricas:: Int -> [(Int,Int,Int)]
ternasPitagoricas = error "Implementar"

-- |5| Función isSubSet: Recibe dos listas y nos dice si la primera lista es subconjunto de la segunda. 
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

-- |8| Función pre: Devuelve el resultado de recorrer un arbol en pre-orden.
pre :: Tree a -> [a]
pre = error "Implementar"