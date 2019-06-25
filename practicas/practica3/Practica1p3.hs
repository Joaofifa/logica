module Practica1p3 where
import Data.List

data Graph = Graph { gId :: Int, 
                     nodes :: [String],
                     edges :: [(String, String)]
                    } deriving (Show, Eq)

{- |1| Función neighbors: Recibe un nodo y una gráfica. Regresa la lista de
   nodos que son vecinos del nodo dado como parámetro.
-}
neighbors :: String -> Graph -> [String]
neighbors s (Graph {gId = gId, nodes = n, edges = []}) = []
neighbors s (Graph {gId = gId, nodes = n, edges = ((x,y):xs)})
   | s == x = y:(neighbors s (Graph { gId = gId, nodes = n, edges = xs}))
   | s == y = x:(neighbors s (Graph {gId = gId, nodes = n, edges = xs}))
   | otherwise = (neighbors s (Graph {gId= gId, nodes = n, edges = xs}))

{- |2| Función mindegree: Recibe una gráfica. Regresa el grado menor de la 
   gráfica.
-}
mindegree :: Graph -> Int
mindegree (Graph{gId = gId, nodes = [], edges = e}) = 0
mindegree (Graph{gId = dId, nodes = [x], edges = e})= contarGrafica x e
mindegree (Graph{gId = gId, nodes = (x:xs) , edges = e}) = 
    min (contarGrafica x e) (mindegree (Graph{gId = gId, nodes = (xs) , edges = e}) )

{- |3| Función maxdegree: Recibe una gráfica. Regresa el grado mayor de la
   gráfica.
-}
maxdegree :: Graph -> Int 
maxdegree (Graph{gId = gId, nodes = [], edges = e}) = 0
maxdegree (Graph{gId = dId, nodes = [x], edges = e})= contarGrafica x e
maxdegree (Graph{gId = gId, nodes = (x:xs) , edges = e}) = 
    max (contarGrafica x e) (maxdegree (Graph{gId = gId, nodes = (xs) , edges = e}) )

{- |4| Función path: Recibe dos nodos y una gráfica. Nos dice si existe un
   camino entre ambos nodos.
-}
path :: String -> String -> Graph -> Bool
path _ _ (Graph {gId = gId, nodes = n, edges = []}) = False
path nodoInicial nodoFinal grafica
                | (elemm nodoInicial (neighbors nodoFinal grafica)) = True
                | otherwise = (auxPath grafica nodoInicial (neighbors nodoFinal grafica))

{- |5| Funciónn delete: Recibe un nodo y una gráfica. Elimina el nodo de la 
   gráfica.
-}
delete :: String -> Graph -> Graph
delete a (Graph {gId = gId, nodes = (xs), edges = e}) = 
    (Graph {gId = gId, nodes = (deleteElemento a xs), edges = (deleteEdges a e)})

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

{-|6| Función simplify: Recibe una expresión proposicional P. Regresa una
      expresión proposional P' equivalente a P sin dobles negaciones en P'.
-}
simplify :: Prop -> Prop
simplify phi = case phi of
    Top -> Top 
    Bot -> Bot
    Var p -> Var p
    Neg p -> simplify p
    Conj p q -> Conj (simplify p) (simplify q)
    Disy p q -> Disy (simplify p) (simplify q)
    Impl p q -> Impl (simplify p) (simplify q)
    Syss p q -> Syss (simplify p) (simplify q)

{- |7| Función deleteImpl: Recibe una expresión propocional P. Regresa una
       expresión propocional P' equivalente a P sin conectivos de implicación
       en P'.
-}
deleteImpl :: Prop -> Prop
deleteImpl phi = case phi of
    Top -> Top
    Bot -> Bot
    Var p -> Var p 
    Neg p -> Neg (deleteImpl p)
    Conj p q -> Conj (deleteImpl p) (deleteImpl q)
    Disy p q -> Disy (deleteImpl p) (deleteImpl q)
    Impl p q -> Disy (Neg (deleteImpl p)) (deleteImpl q)
    Syss p q -> 
        Disy (Conj (deleteImpl p) (deleteImpl q)) 
        (Conj (Neg (deleteImpl p)) (Neg (deleteImpl q)))

{- |8| Función demorgan: Recibe una expresión proposicional P. Mediante
   equivalencia de las reglas de De Morgan, regresa una expresión proposicional
   P' equivalente a P. 
-}
demorgan :: Prop -> Prop
demorgan phi = case phi of 
    Top -> Top
    Bot -> Bot
    Var p -> Var p
    Neg (Var p) -> Neg (demorgan (Var p))
    Neg (Conj p q) -> Disy (Neg (demorgan p)) (Neg (demorgan q))
    Neg (Disy p q) -> Conj (Neg (demorgan p)) (Neg (demorgan q))
    Conj p q -> Conj (demorgan p) (demorgan q)
    Disy p q -> Disy (demorgan p) (demorgan q)
    Impl p q -> Impl (demorgan p) (demorgan q)
    Syss p q -> Syss (demorgan p) (demorgan q)

{- |9| Función interp. Recibe una fórmula (phi) y un estado e. Regresa la 
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

{- |10| Función fnc: Recibe una expresión proposicional P. Regresa la Forma
   Normal Conjuntiva de una expresión proposicional P.
-}
fnc :: Prop -> Prop
fnc phi = case (fnn phi) of 
    Top -> Top
    Bot -> Bot 
    Var p -> Var p
    Neg p -> Neg (fnc p)
    Conj p q -> Conj (fnc p) (fnc q)
    Disy p q -> distri (fnc p) (fnc q)

{- |11| Función esFNC: Recibe una expresión proposicional P. Nos dice si una
   expresión proposicional P es de la Forma Normal Conjuntiva.
-}
esFNC :: Prop -> Bool
esFNC phi = case phi of
    Top -> True 
    Bot -> True 
    Var p -> True 
    Neg p -> case p of
        (Var k) -> True
        _ -> False 
    Conj p q-> isClause p && isClause q 
    _ -> False 

{- |12| Función correcto. Recibe una lista de fórmulas proposicionales (gamma) 
   y una conclusión (phi). Nos dice si el argumento es lógicamente correcto o 
   no.

   Descripción:
   Un argumento con premisas A1, A2, ..., An y una conclusión B es lógicamente
   correcto si {A1, A2, ..., An} es consecuencia lógica de B. Así, basta saber
   si (gamma) es consecuencia lógica de (phi).

   Ejemplos de entrada:
   *PracticaN> correcto [Impl(Var 'P')(Var 'Q'), Impl(Var 'R')(Var 'S'), 
   Disy(Var 'P')(Var 'R'), Neg(Conj(Var 'Q')(Var 'S'))] 
   (Impl(Var 'P')(Conj(Neg(Var 'R'))(Var 'S')))
   False

   *PracticaN> correcto [Impl(Var 'P')(Var 'Q'), Impl(Var 'Q')(Var 'R'), 
   Disy(Conj(Var 'P')(Neg(Var 'R')))(Conj(Var 'R')(Neg(Var 'P')))] (Var 'R')
   True
-}
correcto :: [Prop] -> Prop -> Bool
correcto gamma phi = consecuencia gamma phi

-- Funciones auxiliares. 

{- |Aux. 1| Funcion contarGrafica: Cuenta el numero de aristas que tiene un 
   vertice.
-}
contarGrafica :: String -> [(String,String)] -> Int
contarGrafica _ [] = 0
contarGrafica e ((x1,x2):xs)
   | e == x1 || e == x2 = 1 + contarGrafica e xs
   | otherwise = contarGrafica e xs

{- |Aux. 2| Funcion deleteElemento: Elimina un elemento de una lista.
-}
deleteElemento :: String -> [String] -> [String]
deleteElemento a [] = []
deleteElemento a (x:xs) 
   | a == x = deleteElemento a xs
   | otherwise = x: deleteElemento a xs

{- |Aux. 3| Funcion contarEdges: Elimina todo los parejas que tiene un 
   elemento.
-}
deleteEdges :: String -> [(String, String)] -> [(String, String)]
deleteEdges a [] = []
deleteEdges a ((x1,x2):xs)
   | a == x1 || a ==x2 = deleteEdges a xs
   | otherwise = ((x1,x2):deleteEdges a xs)

{- |Aux. 4 | Función buscaBool. Recibe una variable proposicional varP, y 
   un estado e = [(VarP, Bool)]. Regresa la segunda componente del primer par 
   ordenado de la lista de estados I, cuyo primer componente sea igual a la
   variable varP. Es decir, regresa el valor booleano asociado a la primer 
   variable proposicional varP que encuentre.

   Ejemplos de entrada:
   *Main> buscaBool 'r' [('p', True), ('q', True), ('r', False)]
   False

   *Main> buscaBool 'x' [('l', False), ('x', True), ('m', True), ('s', True)]
   True
-}
buscaBool :: (Eq varP) => varP -> [(varP, Bool)] -> Bool
buscaBool v e = head [b | (v',b) <- e, v == v']
    
{- |Aux. 4| Función isClause: Recibe una expresión proposicional P. Nos dice
   si una expresión proposicional P es una cláusula.
-}
isClause :: Prop -> Bool
isClause phi = case phi of
    Top -> True 
    Bot -> True 
    Var p -> True 
    Neg p -> case p of
        (Var k) -> True
        _ -> False 
    Disy (Var p) (Var q) -> True 
    Disy p q -> isClause p && isClause q 
    _ -> False 

{- |Aux. 5| Función fnn: Recibe una expresión proposicional P. Regresa una
   expresión proposicional P' equivalente a P tal que P' es la Forma Normal
   Negativa de P.
-}
fnn :: Prop -> Prop
fnn phi = case deleteImpl phi of 
    Var p -> Var p
    Neg (Var p) -> Neg (fnn (Var p))
    Neg (Conj p q) -> fnn (demorgan (Neg (Conj p q)))
    Neg (Disy p q) -> fnn (demorgan (Neg (Disy p q )))
    Neg (Neg (Var p)) -> fnn (simplify (Neg (Neg (Var p))))
    Conj p q -> Conj (fnn p) (fnn q)
    Disy p q -> Disy (fnn p) (fnn q)

{- |Aux. 6| Función distri: Recibe una expresión proposicional P. Regresa una
   expresión proposicional P' equivalente a P tal que P' es el resultado de 
   aplicarle las leyes distributivas de conjunción y disyunción a P. 
-}
distri :: Prop -> Prop -> Prop 
distri phi psi = case (phi, psi) of 
    (Conj phi psi, n) -> Conj (distri phi n) (distri phi n)
    (n, Conj phi psi) -> Conj (distri n psi) (distri n psi)
    (phi, psi) -> Disy phi psi

{- |Aux. 7| Función varsConj. Recibe una lista de fórmulas (gamma). Regresa la
   lista de variables proposicionales que figuran en todas las fórmulas que 
   están en gamma.

   Descripción:
   Utilizamos una lista de comprensión para obtener todas las variables que 
   figuran en las fórmulas que contiene gamma. Utilizamos concat para obtener
   la lista de listas que tenemos después de aplicar la función "vars" a cada
   una de las fórmulas que tiene gamma.

   Ejemplos de entrada:
   *PracticaN> varsConj [Impl(Var 'P')(Var 'Q'), Disy(Var 'P')(Var 'R')]
   "PQPR"

   *PracticaN> varsConj [Conj(Neg(Var 'J'))(Var 'A'), Syss(Var 'D')(Var 'K'), 
   Disy(Var 'K')(Var 'P'), Impl(Var 'R')(Var 'M'), Neg(Var 'A')]
   "JADKKPRMA"
-}
varsConj :: [Prop] ->[VarP]
varsConj gamma = concat [vars psi | psi <- gamma]

{- |Aux. 8| Función estadosConj. Recibe una lista de fórmulas (gamma). Regresa
   la lista con los estados posibles para (phi).

   Descripción:
   Para obtener la lista de estados, debemos obtener el subconjunto de listas
   de la lista de variables proposicionales de (gamma), hacer las combinaciones
   posibles entre los estados y los valores booleanos e ir concatenando las
   tuplas para formar una lista (que es justo lo que hacemos en la función
   "subconj"), Y como subconj trabaja con listas de comprensión, al final 
   tendremos una lista de listas con los estados deseados.

   Ejemplos de entrada:
   *PracticaN> estadosConj [Disy(Neg(Var 'P'))(Var 'Q'), Impl(Var 'R')(Var 'S')]
   [[('P',True),('Q',True),('R',True),('S',True)], 
   [('P',True),('Q',True),('R',True),('S',False)],
   [('P',True),('Q',True),('R',False),('S',True)],
   [('P',True),('Q',True),('R',False),('S',False)],
   [('P',True),('Q',False),('R',True),('S',True)],
   [('P',True),('Q',False),('R',True),('S',False)],
   [('P',True),('Q',False),('R',False),('S',True)],
   [('P',True),('Q',False),('R',False),('S',False)],
   [('P',False),('Q',True),('R',True),('S',True)],
   [('P',False),('Q',True),('R',True),('S',False)],
   [('P',False),('Q',True),('R',False),('S',True)],
   [('P',False),('Q',True),('R',False),('S',False)],
   [('P',False),('Q',False),('R',True),('S',True)],
   [('P',False),('Q',False),('R',True),('S',False)],
   [('P',False),('Q',False),('R',False),('S',True)],
   [('P',False),('Q',False),('R',False),('S',False)]]
-}
estadosConj :: [Prop] -> [Estado]
estadosConj gamma = subconj (varsConj gamma)
    where subconj [] = [[]]
          subconj (x:xs) = 
            [(x,True):i | i <- subconj xs] ++ [(x,False):i | i <- subconj xs] 

{- |Aux. 9| Función satisfenConj. Recibe un estado e y una lista de fórmulas 
   (gamma). Nos dice si el conjunto de fórmulas es satisfacible con el estado
   dado.

   Descripción:
   Utilizamos una lista de comprensión para obtener la lista de listas con la 
   interpretación de cada una de las fórmulas con el estado e. Utilizamos 
   "and" para determinar si todas las interpretaciones obtenidas son verdaderas
   (en tal caso gamma es satisfacible) o si alguna de ellas es falsa (en tal 
   caso gamma no es satisfacible).

   Ejemplos de entrada:
   *PracticaN> satisfenConj [('S', False), ('P', True), ('Q', False)] 
   [Impl(Var 'P')(Var 'Q'), Conj(Disy(Var 'S')(Var 'P'))(Neg(Var 'Q')), 
   Neg(Var 'S')]
   False

   *PracticaN> satisfenConj [('P', False), ('R', False), ('S', False), 
   ('T', False), ('Q', True)] [Impl(Var 'P')(Neg(Var 'Q')), 
   Impl(Disy(Var 'R')(Var 'S'))(Var 'T'), Impl(Var 'T')(Var 'Q')]
   True
-}
satisfenConj :: Estado -> [Prop] -> Bool
satisfenConj e phi = and [satisfen e psi | psi <- phi]

{- |Aux. 10| Función satisfen. Recibe un estado e y una fórmula (phi). Nos dice
   si (phi) es satisfacible con el estado dado.

   Descripción:
   La función es simple. Si la interpretación de (phi) con el estado dado es 
   verdadera, entonces (phi) es satisfacible por definición. 

   Ejemplos de entrada:
   *PracticaN> satisfen [('P', True), ('R', False), ('S', True)] 
   (Disy(Impl(Var 'P')(Var 'R'))(Conj(Neg(Var 'S'))(Var 'P')))
   True

   *PracticaN> satisfen [('P', True), ('R', False), ('S', True)] 
   (Disy(Impl(Var 'P')(Var 'R'))(Conj(Neg(Var 'S'))(Var 'P')))
   False
-}
satisfen :: Estado -> Prop -> Bool
satisfen i phi = interp phi i == True

{- |Aux. 11| Función consecuencia. Recibe una lista de fórmulas gamma y una 
   conclusión (phi). Nos dice si (phi) es consecuencia lógica de (gamma).

   Descripción:
   Para mostrar que (phi) es consecuencia lógica de (gamma) basta verificar que
   no existen modelos de (gamma) que no sean modelos de (phi), es decir, que la
   lista de modelos de (gamma) que no son modelos de (phi) es vacía. Esto lo 
   hacemos usando una lista de comprensión, donde escribimos las 
   espeficicaciones antes mencionadas.

   Ejemplos de entrada:
   *PracticaN> consecuencia [Impl(Var 'P')(Conj(Var 'Q')(Var 'R'))] 
   (Disy(Impl(Var 'P')(Var 'Q'))(Impl(Var 'P')(Var 'R')))
   True

   *PracticaN> consecuencia [Impl(Var 'P')(Var 'Q'), Impl(Var 'R')(Var 'S'), 
   Disy(Var 'P')(Var 'R'), Neg(Conj(Var 'Q')(Var 'S'))] 
   (Conj(Impl(Var 'Q')(Var 'P'))(Impl(Var 'S')(Var 'R')))
   True
-}
consecuencia :: [Prop] -> Prop -> Bool
consecuencia gamma phi = null [i | i <- estadosConj (phi: gamma), 
                               satisfenConj i gamma, not (satisfen i phi)]    

{- |Aux. 12| Función vars. Recibe una formula (phi). Regresa la lista de 
   variables proposicionales que figuran en (phi), sin repetición.

   Descripción: 
   Simplemente utilizamos la función 'union' para unir las listas finales
   que contienen las variables proposicionales de cada una de las fórmulas. 
   Por cómo está definida 'union', nos regresa la lista que contiene todos 
   los elementos de las listas que recibe, sin repetición.

   Ejemplos de entrada:
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

{- |Aux. 13| Función elemm : Recibe un elemento y una lista. Nos dice si el elemento
   pertenece a la lista pasada como parámetro.

   Descripción: Si la lista entonces nos regresará False como resultado. En otro caso,
   revisa si el elemento x es igual a la cabeza de la lista, de ser así nos regresa True,
   en otro caso se hace recursión sobre la cola de la lista y se repite lo anterior.

   Ejemplos de entrada:
   *PracticaN> elemm 'a' ['a','b']
   True
-}

elemm :: (Eq a) => a -> [a] -> Bool
elemm _ []       = False
elemm x (y:ys)   = x == y || elem x ys

{- |Aux. 14| Función auxPath. Recibe una gráfica, una cadena y una lista de cadenas.
   La cadena representa el nodo inicial y la lista de cadenas sus vecinos. 
   Nos regresa True si entre dos nodos existe un camino y False en otro caso.

   Descripción:
   Si la lista de vecinos es vacía, entonces no existen vestices adyacentes en la
   gráfica y por lo tanto no existe ningún camino, así que la función regresará 
   False como resultado.

   En otro caso, revisa que el nodo inicial pertenezca a la lista de vecinos o
   a los vecinos de sus vecinos.   
-}

auxPath :: Graph -> String -> [String] -> Bool
auxPath grafica nodoInicial [] = False
auxPath grafica nodoInicial (vecinos:xs) = (elemm nodoInicial (neighbors vecinos grafica)) || 
                                           (auxPath grafica nodoInicial xs)