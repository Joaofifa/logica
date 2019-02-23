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
data Prop = TTrue
           | FFalse 
           | Var String
           | Neg Prop
           | Conj Prop Prop
           | Disy Prop Prop
           | Impl Prop Prop
           | Syss Prop Prop
           deriving (Eq,Ord)

type Estado = [(String, Prop)]

{-|6| Función simplify: Recibe una expresión proposicional P. Regresa una
      expresión proposional P' equivalente a P sin dobles negaciones en P'.
-}
simplify :: Prop -> Prop
simplify TTrue = TTrue
simplify FFalse = FFalse
simplify Var v = Var v 
simplify Neg p = Neg p
simplify Conj p q = p conj q

{- |7| Función deleteImpl: Recibe una expresión propocional P. Regresa una
       expresión propocional P' equivalente a P sin conectivos de implicación
       en P'.
-}
deleteImpl :: Prop -> Prop
deleteImpl = error "Implementar"

{- |8| Función demorgan: Recibe una expresión proposicional P. Mediante
   equivalencia de las reglas de De Morgan, regresa una expresión proposicional
   P' equivalente a P. 
-}
demorgan :: Prop -> Prop
demorgan = error "Implementar"

{- |9| Función inter: Recibe una expresión proposicional P y un estado
   booleano. Regresa la interpretación de la expresión proposicional P con el
   estado dado. 
-}
inter :: Prop -> Estado -> Bool
inter = error "Implementar"

{- |10| Función fnc: Recibe una expresión proposicional P. Regresa la Forma
   Normal Conjuntiva de una expresión proposicional P.
-}
fnc :: Prop -> Prop
fnc = error "Implementar"

{- |11| Función esFNC: Recibe una expresión proposicional P. Nos dice si una
   expresión proposicional P es de la Forma Normal Conjuntiva.
-}
esFNC :: Prop -> Bool
esFNC = error "Implementar"

{- Recibe una lista de expresiones proposicionales [P, Q,..] y una conclusión
   A. Nos dice si el argumento es lógicamente correcto o no.
-}
correcto :: [Prop] -> Prop -> Bool
correcto = error "Implementar"
