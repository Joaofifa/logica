-- @autor Rubí Rojas Tania Michelle.
module LogicaProp where
import Data.List

-- Las variables proposicionales son del tipo Char.
type VarP = Char

{- Los estados son listas de tuplas donde la primer componente de la tupla es 
   una variable proposicional y su segundo componente será el valor booleano 
   asociado a dicha variable.
-}
type Estado = [(VarP, Bool)]

-- El tipo de dato para las fórmulas proposicionales.
data Prop = Var VarP  -- Var 'p'
          | Neg Prop   -- ~P
          | Conj Prop Prop -- P ^ Q
          | Disy Prop Prop -- P v Q
          | Impl Prop Prop -- P -> Q
          | Syss Prop Prop -- P <-> Q
          deriving (Eq, Ord, Show)

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
   Las definiciones que usamos para cada una es la aplicación directa de
   nuestra definición de semántica. Notemos dos cosas, la primera, que se 
   utilizó una equivalencia lógica para la interpretación de la implicación,
   y segundo, que se hace uso de una función auxiliar 'buscaBool' cuyo
   objetivo es regresar el valor booleano asociado a la primer aparición de
   la variable proposicional varP que se le pase como parámetro.

   Ejemplos:
   *Main> interp (Conj (Var 'p') (Var 'q')) [('p', True), ('q', False)]
   False

   *Main> interp (Disy (Neg (Var x)) (Neg (Var y))) [('x', False), ('y', True)]
   True
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

   Aquí simplemente utilizamos la función 'union' para unir las listas finales
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
    Var x -> [x]
    Neg p -> vars p
    Conj p q -> vars p `union` vars q
    Disy p q -> vars p `union` vars q
    Impl p q -> vars p `union` vars q
    Syss p q -> vars p `union` vars q

{- |3. Función estados| Recibe una fórmula (phi) con n-variables 
   proposicionales. Regresa la lista de listas con los 2^n estados 
   distintos para (phi).

   Notemos que para obtener los 2^n estados posibles, debemos obtener el 
   subconjunto de listas de la lista de variables proposicionales de 
   (phi), hacer las combinaciones posibles entre los estados y los valores 
   booleanos e ir concatenando las parejas de tuplas para formar una lista
   (que es justo lo que hacemos en la función subconj). Y como subconj trabaja
   con una lista de comprensión, al final obtendremos una lista de listas 
   (ya que los estados son listas) con los 2^n estados.
   
   Ejemplos:
   *Main> estados (Impl (Neg (Var 'q')) (Disy (Var 'r') (Var 'p')))
   [[('q',True),('r',True),('p',True)],[('q',True),('r',True),('p',False)],
   [('q',True),('r',False),('p',True)],[('q',True),('r',False),('p',False)],
   [('q',False),('r',True),('p',True)],[('q',False),('r',True),('p',False)],
   [('q',False),('r',False),('p',True)],[('q',False),('r',False),('p',False)]]

   *Main> estados (Disy (Neg (Var x)) (Neg (Var y)))
   [[('x',True),('y',True)],[('x',True),('y',False)],[('x',False),('y',True)],[('x',False),('y',False)]]
   -}
estados:: Prop -> [Estado]
estados phi = subconj (vars phi)
    where subconj [] = [[]]
          subconj (x:xs) = 
            [(x,True):i | i <- subconj xs] ++ [(x,False):i | i <- subconj xs] 

{- |4. Función varCN} Recibe una fórmula (phi). Regresa la lista de variables 
   proposicionales que figuran en (phi) junto con su conectivo unario (si es
   que tiene).
   
   La diferencia con la función vars es que si la variable porposicional tiene
   una negación, la manda a la lista junto con su conectivo unario.

   Aquí simplemente utilizamos la función 'union' para unir las listas finales
   que contienen las variables proposicionales de cada una de las fórmulas. 
   Por cómo está definida 'union', nos regresa la lista que contiene todos 
   los elementos de las listas que recibe, sin repetición.

   Ejemplos: 
   *Main> varCN (Conj (Neg (Var 'p')) (Var 'r'))
   [Neg (Var 'p'),Var 'r']

   *Main> varCN (Neg (Conj (Var 'p') (Neg (Var 'q'))))
   [Var 'p',Neg (Var 'q')]
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

{- |5. Función sonDiferentes| Recibe una fórmula (phi) con únicamente dos
   variables proposicionales y un estado e. Nos dice si la interpretación de
   cada una de sus variables con el estado e son diferentes.

   En esta función es donde 'varCN' cobra mucha importancia. Sabemos que las 
   variables proposicionales de (phi) pueden estar solas o negadas, por lo que
   no nos sirve usar la función 'vars' ya que no estaríamos considerando el 
   caso en que las variables están negadas. Así, utilizamos la función 'varCN' 
   para evaluar ambas variables de la fórmula (phi) y verificar si la 
   interpretación de éstas es diferente o no. Esta función es fácil de 
   realizar ya que únicamente hay que checar si la interpretación de la
   primer variable de (phi) es diferente a la interpretación de la segunda
   variable de (phi), con el estado dado.

   Ejemplos:
   *Main> sonDiferentes (Conj (Var 'p') (Neg (Var 'q'))) 
    [('p', True), ('q', False)]
   False

   *Main> sonDiferentes (Impl (Var 'r') (Neg (Var 's'))) 
   [('r', True), ('s', False)]
   False

   *Main> sonDiferentes (Syss (Var 'q') (Var 'p')) 
   [('p', False), ('q', True)]
   True
-}
sonDiferentes :: Prop -> Estado -> Bool
sonDiferentes phi e = 
    (interp (head (varCN phi)) e) /= (interp (last (varCN phi)) e)

{- |6. Función sonIguales| Recibe una fórmula (phi) con únicamente dos 
   variables proposicionales y un estado e. Nos dice si la interpretación de
   cada una de sus variables con el estado e son iguales.

   EEn esta función es donde 'varCN' cobra mucha importancia. Sabemos que las 
   variables proposicionales de (phi) pueden estar solas o negadas, por lo que
   no nos sirve usar la función 'vars' ya que no estaríamos considerando el 
   caso en que las variables están negadas. Así, utilizamos la función 'varCN' 
   para evaluar ambas variables de la fórmula (phi) y verificar si la 
   interpretación de éstas es igual o no. Esta función es fácil de 
   realizar ya que únicamente hay que checar si la interpretación de la
   primer variable de (phi) es igual a la interpretación de la segunda
   variable de (phi), con el estado dado.

   Ejemplos:
   *Main> sonIguales (Conj (Var 'p') (Neg (Var 'q'))) 
   [('p', True), ('q', False)]
   True

   *Main> sonIguales (Impl (Var 'r') (Neg (Var 's'))) 
   [('r', True), ('s', False)]
   True

   *Main> sonIguales (Syss (Var 'q') (Var 'p')) 
   [('p', False), ('q', True)]
   False
-}
sonIguales :: Prop -> Estado -> Bool
sonIguales beta e = 
    (interp (head (varCN beta)) e) == (interp (last (varCN beta)) e)

-- Funciones auxiliares. --

{- |1. Función auxiliar buscaBool| Recibe una variable proposicional varP, y 
   un estado [(varP,Bool)]. Regresa la segunda componente del primer par 
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
buscaBool varP e = head [b | (x,b) <- e, varP == x]
