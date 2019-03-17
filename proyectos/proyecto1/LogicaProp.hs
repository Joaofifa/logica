-- @autor Rubí Rojas Tania Michelle.
module LogicaProp where
import Data.List

-- Las variables proposicionales son del tipo Char.
type VarP = Char

-- El tipo de dato para las fórmulas proposicionales.
data Prop = Var VarP  -- Var 'p'
          | Neg Prop   -- ~P
          | Conj Prop Prop -- P ^ Q
          | Disy Prop Prop -- P v Q
          | Impl Prop Prop -- P -> Q
          | Syss Prop Prop -- P <-> Q
            deriving (Eq, Ord, Show)

{- Los estados son listas de tuplas donde la primer componente de la tupla es 
   una variable proposicional y su segundo componente será el valor asociado a 
   dicha variable.
-}
type Estado = [(VarP, Bool)]

-- Ejemplos de variables proposicionales:
x, y, z :: VarP 
x = 'x'
y = 'y'
z = 'z'

-- Ejemplos de fórmulas:
alpha, gamma, taf :: Prop
alpha = (Conj (Var x) (Var y))
gamma = (Impl (Conj (Var z) (Var y)) (Disy (Var y) (Var z)))
taf = (Disy (Neg (Var x)) (Neg (Var y)))

{- |1. Función interp|  Recibe una fórmula (phi) y un estado e. Regresa la 
   interpretación de phi con el estado dado.
   Ejemplo:
   *Main> interp (Conj (Var 'p') (Var 'q')) [('p', True), ('q', False)]
   False
-}
interp :: Prop -> Estado -> Bool
interp phi e = case phi of 
    Var i -> buscaBool i e
    Neg p -> not (interp p e)
    Conj p q -> (interp p e) && (interp q e)
    Disy p q -> (interp p e) || (interp q e)
    Impl p q -> (not (interp p e)) || (interp q e)
    Syss p q -> (interp p e) == (interp q e)

{- |2. Función vars| Recibe una formula (phi). Regresa la lista de variables 
   proposicionales que figuran en (phi), sin repetición.
   Ejemplo: 
   *Main> vars (Syss (Impl (Var 'p') (Var 'r')) (Conj (Var 'q') (Var 's')))
   "prqs"
-}
vars :: Prop -> [VarP]
vars phi = case phi of
    Var x -> [x]
    Neg p -> vars p
    Conj p q -> vars p `union` vars q
    Disy p q -> vars p `union` vars q
    Impl p q -> vars p `union` vars q
    Syss p q -> vars p `union` vars q

{- |3. Función estados| Recibe una fórmula (phi) con n-variables 
   proposicionales. Regresa la lista con los 2^n estados distintos para (phi).
   Ejemplo:
   *Main> estados (Impl (Neg (Var 'q')) (Disy (Var 'r') (Var 'p')))
   [[('q',True),('r',True),('p',True)],[('q',True),('r',True),('p',False)],
   [('q',True),('r',False),('p',True)],[('q',True),('r',False),('p',False)],
   [('q',False),('r',True),('p',True)],[('q',False),('r',True),('p',False)],
   [('q',False),('r',False),('p',True)],[('q',False),('r',False),('p',False)]]
-}
estados:: Prop -> [Estado]
estados phi = subconj (vars phi)
    where subconj [] = [[]]
          subconj (x:xs) = 
            [(x,True):i | i <- subconj xs] ++ [(x,False):i | i <- subconj xs] 

{- |4. Función varCN} Recibe una fórmula (phi). Regresa la lista de variables 
   proposicionales que figuran en (phi). La diferencia con la función vars es
   que si la variable porposicional tiene una negación, la manda a la lista 
   junto con su conectivo unario.
   Ejemplo: 
   *Main> varCN (Conj (Neg (Var 'p')) (Var 'r'))
   [Neg (Var 'p'),Var 'r']
-}
varCN :: Prop -> [Prop]
varCN phi = case phi of
    Var x -> [Var x]
    (Neg (Var i)) -> [Neg (Var i)]
    Neg p -> varCN p
    Conj p q -> varCN p `union` varCN q
    Disy p q -> varCN p `union` varCN q
    Impl p q -> varCN p `union` varCN q
    Syss p q -> varCN p `union` varCN q

-- Funciones auxiliares. --

{- |1. Función auxiliar buscaBool| Recibe una variable proposicional p, y un
   estado [(p,b)]. Regresa la segunda componente del primer par ordenado de
   la lista de estados I, cuyo primer componente sea igual a la variable p.
   Ejemplo:
   *Main> 'r' [('p', True), ('q', True), ('r', False)]
   False
-}
buscaBool :: (Eq p) => p -> [(p,b)] -> b 
buscaBool p e = head [b | (x,b) <- e, p == x]
