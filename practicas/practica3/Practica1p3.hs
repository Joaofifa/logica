module Practica1p3 where

data Graph = Graph { gId :: Int, 
					nodes :: [String],
					edges :: [(String, String)]
                    } deriving (Show, Eq)

{- |1| Función neighbors: 
-}
neighbors :: String -> Graph -> [String]
neighbors = error "Implementar"

{- |2| Función mindegree:
-}
mindegree :: Graph -> Int
mindegree = error "Implementar"

{- |3| Función maxdegree: 
-}
maxdegree :: Graph -> Int 
maxdegree = error "Implementar"

{- |4| Función path: 
-}
path :: String -> String -> Graph -> Bool
path = error "Implementar"

{- |5| Funciónn delete: 
-}
delete :: String -> Graph -> Graph
delete = error "Implementar"

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
    Neg (Var p) -> Neg (simplify (Var p))
    Neg (Neg (Var p)) -> simplify (Var p)
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
    Syss p q -> (inter p [(valor, k)]) && (inter q [(valor, k)]) 

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
-}
correcto :: [Prop] -> Prop -> Bool
correcto = error "Implementar"

-- Funciones auxiliares. 

{- |Aux. 1| Función isClause: Recibe una expresión proposicional P. Nos dice
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

{- |Aux. 2| Función fnn: Recibe una expresión proposicional P. Regresa una
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

{- |Aux. 3| Función lawDistri: Recibe una expresión proposicional P. Regresa una
   expresión proposicional P' equivalente a P tal que P' es el resultado de 
   aplicarle las leyes distributivas de conjunción y disyunción a P. 
-}
distri :: Prop -> Prop -> Prop 
distri phi psi = case (phi, psi) of 
    (Conj phi psi, n) -> Conj (distri phi n) (distri phi n)
    (n, Conj phi psi) -> Conj (distri n psi) (distri n psi)
    (phi, psi) -> Disy phi psi