module Practica1p3 where

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

-- Tipo de dato para representar expresiones de la lógica proposicional.
data Prop = Top -- True
           | Bot -- False
           | Var String -- Var P
           | Neg Prop -- neg P
           | Conj Prop Prop -- (P ∧ Q)
           | Disy Prop Prop -- (P ∨ Q)
           | Impl Prop Prop -- (P → Q)
           | Syss Prop Prop -- (P ↔ Q)
           deriving (Eq, Ord, Show)

type Estado = [(String, Prop)]

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

{- |9| Función inter: Recibe una expresión proposicional P y un estado
   booleano. Regresa la interpretación de la expresión proposicional P con el
   estado dado. 
-}
inter :: Prop -> Estado -> Bool
inter phi [(valor, k)] = case phi of 
    Top -> True 
    Bot -> False 
    Var p -> elem p [valor] 
    Neg p -> not (inter p [(valor, k)])
    Conj p q -> (inter p [(valor, k)]) && (inter q [(valor, k)])
    Disy p q -> (inter p [(valor, k)]) || (inter q [(valor, k)])
    Impl p q -> not (inter p [(valor, k)]) || (inter q [(valor, k)])
    Syss p q -> (inter p [(valor, k)]) == (inter q [(valor, k)]) 

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

{- |12| Función correcto: Recibe una lista de expresiones proposicionales 
   [P, Q,..] y una conclusión C. Nos dice si el argumento es lógicamente 
   correcto o no.
correcto :: [Prop] -> Prop -> Bool
correcto gamma phi = consecuencia gamma phi
-}

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

{- |Aux. 7| Función consecuencia: Recibe una lista de expresiones 
   proposicionales [P, Q,..] y una conclusión C. Nos dice si la conclusión
   es consecuencia lógica de los argumentos.
-}
consecuencia :: [Prop] -> Prop -> Bool
consecuencia gamma phi = insatisfConj ((Neg phi) : gamma)

{- |Aux. 10| Función estados: Recibe una expresión proposicional. Devuelve
   la lista con los 2^n estados distintos para la expresión proposicional.
-}
estados :: Prop -> [Prop]
estados psi = vars psi

{- |Aux. 11| Función vars: Recibe una expresión proposicional. Regresa la lista
   de variables proposicionales que figuran en la expresión proposicional.
-}
vars :: Prop -> [Prop]
vars phi = case phi of
    Top -> []
    Bot -> [] 
    Var p -> [Var p]
    Neg p -> vars(p)
    Conj p q -> vars (p) ++ vars (q)
    Disy p q -> vars (p) ++ vars (q)
    Impl p q -> vars (p) ++ vars (q)
    Syss p q -> vars (p) ++ vars (q)

{- |Aux. 11| Función elemm : Recibe un elemento y una lista. Nos dice si el elemento
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

{- |Aux. 9| Función auxPath. Recibe una gráfica, una cadena y una lista de cadenas.
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