module PracticaN where
import Data.List

-- Las variables proposicionales son del tipo Char.
type VarP = Char

{- Los estados son listas de tuplas donde la primer componente de la tupla es 
   una variable proposicional y su segundo componente será el valor booleano 
   asociado a dicha variable.
-}
type Estado = [(VarP, Bool)]

-- El tipo de dato para representar las fórmulas proposicionales.
data Prop = Top -- True
          | Bot -- False
          | Var VarP  -- Var "R"
          | Neg Prop   -- ~P
          | Conj Prop Prop -- P ^ Q
          | Disy Prop Prop -- P v Q
          | Impl Prop Prop -- P -> Q
          | Syss Prop Prop -- P <-> Q
          deriving (Eq,  Ord, Show)

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
   un estado e = [(VarP, Bool)]. Regresa la segunda componente del primer par 
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
buscaBool v e = head [b | (v',b) <- e, v == v']

{- |Aux. 2| Función interp. Recibe una fórmula (phi) y un estado e. Regresa la 
   interpretación de phi con el estado dado.

   Descripción:
   Siguiendo nuestra definición de interpretación, sabemos que
   - I(v) = v, donde v es una variable proposicional. 
   - I(Top) = 1
   - I(Bot) = 0
   - I(~P) = 1 <-> I(P) = 0
   - I(P ^ Q) = 1 <-> I(P) = I(Q) = 1
   - I(P v Q) = 0 <-> I(P) = I(Q) = 0
   - I(P -> Q) = 0 <-> I(P) = 1 e I(Q) = 0
   - I(P <-> Q) = 1 <-> I(P) = I(Q)
   
   donde la única modificación que hacemos en nuestra implementación es para 
   el operador Impl, ya que utilizamos una equivalencia lógica para facilitar
   el cálculo de la interpretación de dicho operador.

   Ejemplos de entrada:
   *Main> interp (Conj (Var 'p') (Var 'q')) [('p', True), ('q', False)]
   False

   *Main> interp (Disy (Neg (Var 'x')) (Neg (Var 'y'))) [('x', False), ('y', True)]
   True
-}
interp :: Prop -> Estado -> Bool
interp phi e = case phi of 
    Top -> True
    Bot -> False 
    Var v -> buscaBool v e
    Neg p -> not (interp p e)
    Conj p q -> (interp p e) && (interp q e)
    Disy p q -> (interp p e) || (interp q e)
    Impl p q -> not (interp p e) || (interp q e)
    Syss p q -> (interp p e) == (interp q e)

{- |Aux. 3| Función vars. Recibe una formula (phi). Regresa la lista de 
   variables proposicionales que figuran en (phi), sin repetición.

   Descripción: 
   Simplemente utilizamos la función 'union' para unir las listas finales
   que contienen las variables proposicionales de cada una de las fórmulas. 
   Por cómo está definida 'union', nos regresa la lista que contiene todos 
   los elementos de las listas que recibe, sin repetición.

   Ejemplos: 
   *Main> vars (Syss (Impl (Var 'p') (Var 'r')) (Conj (Var 'q') (Var 's')))
   "prqs"

   *Main vars (Disy (Neg (Var 'x')) (Neg (Var 'y')))
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

{- |Aux. 4| Función varsConj.
-}
varsConj :: [Prop] ->[VarP]
varsConj phi = concat [vars psi | psi <- phi]

{- |Aux. 5|
-}
estadosConj :: [Prop] -> [Estado]
estadosConj phi = subconj (varsConj phi)
    where subconj [] = [[]]
          subconj (x:xs) = 
            [(x,True):i | i <- subconj xs] ++ [(x,False):i | i <- subconj xs] 

{- |Aux. 6|
-}
satisfenConj :: Estado -> [Prop] -> Bool
satisfenConj e phi = and [satisfen e psi | psi <- phi]

{- |Aux. 7|
-}
satisfen :: Estado -> Prop -> Bool
satisfen i phi = interp phi i == True

{- |Aux. 8|
-}
consecuencia :: [Prop] -> Prop -> Bool
consecuencia gamma phi = null [i | i <- estadosConj (phi: gamma), 
                               satisfenConj i gamma, not (satisfen i phi)]