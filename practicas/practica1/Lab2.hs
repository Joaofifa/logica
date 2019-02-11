module Lab2 where

-- Laboratorio 7 de Febrero, 2019.

-- Función isEmpty: Nos dice si una lista es la lista vacía.
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

-- Función takeL: Toma los primeros n elementos de una lista.
takeL :: Int -> [a] -> [a]
takeL  0 _ = []
takeL _ [] =[]
takeL n (x:xs) = x:takeL (n-1) xs

-- Función dropL: Elimina los primeros n elementos de una lista.
dropL :: Int -> [a] -> [a]
dropL _ [] = []
dropL 0 (x:xs) = x:xs  
dropL n (x:xs) = dropL (n-1) xs  

-- Función headL: Regresa el primer elemento de una lista.
headlL :: [a] -> a
headlL [] = error "Empty liist"
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
{- Definición creada por ustedes :)
initL [] = error " "
initL (x:[]) = []
initL (x:xs) = x:initL xs 
-}

-- Función lengthL: Regresa el tamaño de una lista.
lengthL :: [a] -> Int
lengthL [] = 0
lengthL (x:xs) = 1 + lengthL (xs)

{-
Todos los números del 0 al 1000 que sean múltiplos de 13.
[x| x <- [0 .. 1000], mod x 13 == 0]
Todos los números del 0 al 1000 que sean múltiplos de 13 y su último digito sea 2.
[x|x <- [0..1000],mod x 13 == 0, mod x 10 == 2]
-}