module Lab3 where 

{- Función iniciooracion: La primera mayuscula la convierte a mayusucla y el resto en minuscula.
iniciooracion "gRacE"
> Grace
-}
iniciooracion :: String -> String
iniciooracion [] = []
iniciooracion (x:xs) = toUpper x:[toLower x|x <- xs] 

-- emp: Recibe dos listas y va emparejando los elementos con mismo índice. Regresa una lista de duplas.
emp :: [a] -> [a] -> [(a,a)]
emp = error "Implementar"


inversion :: [a] -> [a]
inversion ls = case ls of
 [] -> []
 [x] -> [x]
 x:xs -> inversion xs ++ [x]

{-
0!! = 1
1!! = 1
n!! = n*(n-2)*...*3*1, si n es impar
n!! = n*(n-2)*...*4*2, si n es par
-}
-- Función dFac: Recibe un número y calcula su doble factorial.
dFac :: Int -> Int
dFac = error "Implementar"

{- Junta los primeros elementos, los segundos, etc. de tal forma que las longitudes son iguales a la mas corta
junta [[1,2,3],[7,8,9],[10,11,12,13,14,15,16]]
> [[1,7,10],[2,8,11],[3,9,12]]
-}
junta :: Eq a => [[a]] -> [[a]]
junta = error "Implementar"

-- foldl y foldr
-- map :: (a -> b) -> [a] -> [b]


data ArbolB a = VacioB | NodoB a (ArbolB a) (ArbolB a) deriving Show

-- Invierte una lista. Se tiene que hacer uso específico de foldr
inversionL :: [a] -> [a]
inversionL = error "Implementar"

-- Función high: Devuelve la altura de un árbol binario.
high :: ArbolB a -> Int
high VacioB = 0
high (NodoB _ l r) = 1 + max(high l) (high r)

element :: Eq a => a -> ArbolB a -> Bool
element e VacioB = False
element e (NodoB a l r) = if a == e
						  then True
						  else (element e l) || (element e r)

hojas :: ArbolB a -> Int
hojas VacioB = 0
hojas (Nodo a VacioB VacioB) = 1
hojas (Nodo a l r) = hojas l + hojas r

insertTree :: (Eq a, Ord a) => a -> ArbolB a -> ArbolB a 
insertTree e VacioB = Nodo e VacioB VacioB
insertTree e (Nodo ea i d) 
 | e <= ea = NodoB ea (insertTree e i) d
 | otherwise = NodoB ea i (insertTree e d)