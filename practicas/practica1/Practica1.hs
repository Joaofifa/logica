module Practica1 where

-- |1| Función getNth: 
getNth :: Int -> [a] -> a
getNth = error "Implementar"

{- |2| Función update: Recibe un número, un elemento y una lista. Actualiza 
   el n-ésimo elemento de la lista.
-}
update :: Int -> a -> [a] -> [a]
update = error "Implementar"
                                                                                .
{- |3| Función dropP: Se prohíbe el uso de las funciónes TAKE y DROP.
-}
dropP :: Int -> [a] -> [a]
dropP = error "Implementar"

{- |4| Función split: Recibe un número y una lista. Separa una lista en dos 
   partes, tomando en cuenta el entero dado. Se obtiene una lista de listas.
   Se prohíbe el uso de las funciónes TAKE y DROP.
   Hint: Usar las funciones takeL y dropL vistas en el laboratorio.
-}
split :: Int -> [a] -> [[a]]
split = error "Implementar"

-- |5| Función range: 
range :: Int -> Int -> [Int]
range = error "Implementar"

{- |6| Función filterP: Regresa los elementos de una lista que cumplen un 
   predicado.  Se prohíbe el uso de la función FILTER.
-}
filterP :: (a -> Bool) -> [a] -> [a]
filterP = error "Implementar"

{-|7| Función rotate: Rota n lugares de una lista.-}
rotate :: Int -> [a] -> [a]
rotate = error "Implementar"

-- |8| Función palindrome: Nos dice si una lista es palindroma o no.
palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome [a] = True
palindrome (x:xs) =
    if (headlL (x:xs) == lastL (x:xs)) then palindrome (initL (tailL (x:xs)))  
    else False

-- Función headL: Regresa el primer elemento de una lista.
headlL :: [a] -> a
headlL [] = error "Lista vacía."
headlL (x:xs) = x

-- Función lastL: Regresa el úlitmo elemento de una lista.
lastL :: [a] -> a
lastL [] = error " "
lastL (x:[]) = x
lastL (x:xs) = lastL xs 

-- Función tailL: Regresa la cola de una lista.
tailL :: [a] -> [a]
tailL [] = error ""
tailL (x:xs) = xs 

-- Función initL: Regresa la lista sin el último elemento.
initL :: [a] -> [a]
initL [] = error " D="
initL (x:xs) = if (isEmpty xs) then [] else x:(initL xs)

-- Función isEmpty: Nos dice si una lista es la lista vacía.
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

{- |9| Función intercala: Recibe un elemento y una lista de elementos. 
   Devuelve la lista de listas con el elemento intercalado en una lista.
-}
intercala :: a -> [a] -> [[a]]
intercala = error "Implementar"
 

{- |10| Función factorial: Recibe un número. Si el número es negativo, regresa
   un error. En caso contrario, regresa el factorial del número.
-} 
factorial :: Int -> Int
factorial 0 = 1
factorial n = 
    if n < 0 then error "El número debe ser un entero positvo." 
    else n*factorial (n-1)  

{- |11| Función repite: Recibe un número y un elemento. Si el número es
   negativo, regresa un error. En caso contrario, regresa una lista con el
   elemento repetivo n veces.
-}
repite :: Int -> a -> [a] 
repite 0 a = []
repite n a = 
    if n < 0 then error "El número debe ser un entero positivo." 
    else (a:repite (n-1) a)

{- |12| Función fib: Recibe un número y regresa la función fibonacci aplicada
   al número.
 -}
fib :: Int -> Int
fib = error "Implementar"