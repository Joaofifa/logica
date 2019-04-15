module PracticaN where
import Data.List

-- Las variables proposicionales son del tipo String.
data VarP = String

{- Los estados son listas de tuplas donde la primer componente de la tupla es 
   una variable proposicional y su segundo componente será el valor booleano 
   asociado a dicha variable.
-}
type Estado = [(VarP, Prop)]

-- El tipo de dato para representar las fórmulas proposicionales.
data Prop = Top -- True
           | Bot -- False 
           | Var VarP -- Var "P"
           | Neg Prop -- neg P
           | Conj Prop Prop -- (P ∧ Q)
           | Disy Prop Prop -- (P ∨ Q)
           | Impl Prop Prop -- (P → Q)
           | Syss Prop Prop -- (P ↔ Q)

-- Ejemplos de variables proposicionales:
x, y, z :: VarP 
x = x
y = y
z = z

-- Ejemplos de fórmulas:
k, m, n :: Prop
k = (Conj (Var x) (Var y))
m = (Impl (Conj (Var z) (Var y)) (Disy (Var y) (Var z)))
n = (Disy (Neg (Var x)) (Neg (Var y)))

-- Instancia Show para mostrar las fórmulas proposicionales.
instance Show Prop where
    show Top = "True"
    show Bot = "False"
    show (Var v) = "Var " ++ show v
    show (Neg p) = "~ " ++ show p
    show (Conj p q) = "(" ++ (show p) ++ " ^ " ++ (show q) ++ ")"
    show (Disy p q) = "(" ++ (show p) ++ " v " ++ (show q) ++ ")"
    show (Impl p q) = "(" ++ (show p) ++ " -> " ++ (show q) ++ ")"
    show (Syss p q) = "(" ++ (show p) ++ "<-> " ++ (show q) ++ ")"

{- Función correcto. Recibe una lista de fórmulas proposicionales y una 
   conclusión. La función nos dice si el argumento es lógicamente correcto 
   o no.

   Descripción:
   Un argumento con premisas A1, A2, ..., An y una conclusión B es lógicamente
   correcto si {A1, A2, ..., An} es consecuencia lógica de B.

   Ejemplos de entrada:
   *Main> correcto [(Syss(Var "P")(Var "Q")), (Var "P")] (Var "Q")
   True

   -

-}
correcto :: [Prop] -> Prop -> Bool
correcto gamma conclusion = consecuencia gamma conclusion

-- Funciones auxiliares --

{- |Aux. 1 | Función buscaBool. Recibe una variable proposicional varP, y 
   un estado [(varP, Prop)]. Regresa la segunda componente del primer par 
   ordenado de la lista de estados I, cuyo primer componente sea igual a la
   variable varP. Es decir, regresa el valor booleano asociado a la primer 
   variable proposicional varP que encuentre.

   Ejemplos:
   *Main> buscaBool 'r' [('p', True), ('q', True), ('r', False)]
   False

   *Main> buscaBool 'x' [('l', False), ('x', True), ('m', True), ('s', True)]
   True
-}
buscaBool :: (Eq varP) => varP -> [(varP, Bool)] -> Bool
buscaBool c t = head [v | (c',v) <- t, c == c']

{- |1. Función interp|  Recibe una fórmula (phi) y un estado e. Regresa la 
   interpretación de phi con el estado dado.

   Descripción:
   Las definiciones que usamos para cada una es la aplicación directa de
   nuestra definición de semántica. Notemos dos cosas, la primera, que se 
   utilizó una equivalencia lógica para la interpretación de la implicación,
   y segundo, que se hace uso de una función auxiliar 'buscaBool' cuyo
   objetivo es regresar el valor booleano asociado a la primer aparición de
   la variable proposicional varP que se le pase como parámetro.

   Ejemplos de entrada:
   *Main> interp (Conj (Var 'p') (Var 'q')) [('p', True), ('q', False)]
   False

   *Main> interp (Disy (Neg (Var x)) (Neg (Var y))) [('x', False), ('y', True)]
   True
-}
interp :: Prop -> Estado -> Bool
interp phi e = case phi of 
    Top -> True
    Bot -> False
    Var i -> buscaBool i e
    Neg p -> not (interp p e)
    Conj p q -> (interp p e) && (interp q e)
    Disy p q -> (interp p e) || (interp q e)
    Impl p q -> not (interp p e) || (interp q e)
    Syss p q -> (interp p e) == (interp q e)

{- |2. Función vars| Recibe una formula (phi). Regresa la lista de variables 
   proposicionales que figuran en (phi), sin repetición.

   Descripción: 
   Simplemente utilizamos la función 'union' para unir las listas finales
   que contienen las variables proposicionales de cada una de las fórmulas. 
   Por cómo está definida 'union', nos regresa la lista que contiene todos 
   los elementos de las listas que recibe, sin repetición.

   Ejemplos: 
   *Main> vars (Syss (Impl (Var 'p') (Var 'r')) (Conj (Var 'q') (Var 's')))
   "prqs"

   *Main vars (Disy (Neg (Var x)) (Neg (Var y)))
   "xy"
-}
vars :: Prop -> [VarP]
vars phi = case phi of
    Top -> []
    Bot -> []
    Var i -> [i]
    Neg p -> vars p
    Conj p q -> vars p `union` vars q
    Disy p q -> vars p `union` vars q
    Impl p q -> vars p `union` vars q
    Syss p q -> vars p `union` vars q

varsConj :: [Prop] ->[String]
varsConj phi = concat [vars psi | psi <- phi]

estadosConj :: [Prop] -> [Estado]
estadosConj phi = subconj (varsConj phi)
    where subconj [] = [[]]
          subconj (x:xs) = 
            [(x,True):i | i <- subconj xs] ++ [(x,False):i | i <- subconj xs] 

satisfenConj :: Estado -> [Prop] -> Bool
satisfenConj e phi = and [satisfen e psi | psi <- phi]

satisfen :: Estado -> Prop -> Bool
satisfen i phi = interp phi i == True

consecuencia :: [Prop] -> Prop -> Bool
consecuencia gamma phi = null [i | i <- estadosConj (phi: gamma), 
                               satisfenConj i gamma, not (satisfen i phi)]