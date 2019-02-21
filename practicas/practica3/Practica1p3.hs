module Practica1p3 where

data Graph = Graph { gId :: Int, 
					nodes :: [String],
					edges :: [(String, String)]
					} deriving (Show, Eq)

neighbors :: String -> Graph -> [String]
neighbors = error "Implementar"

mindegree :: Graph -> Int
mindegree = error "Implementar"

maxdegree :: Graph -> Int 
maxdegree = error "Implementar"

path :: String -> String -> Graph -> Bool
path = error "Implementar"

delete :: String -> Graph -> Graph
delete = error "Implementar"

-- Tipo de dato para representar expresiones de la lógica proposicional
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

simplify :: Prop -> Prop
simplify = error "Implementar"

deleteImpl :: Prop -> Prop
deleteImpl = error "Implementar"

demorgan :: Prop -> Prop
demorgan = error "Implementar"

inter :: Prop -> Estado -> Bool
inter = error "Implementar"

-- Devuelve la forma normal conjuntiva de una fórmula
fnc :: Prop -> Prop
fnc = error "Implementar"

esFNC :: Prop -> Bool
esFNC = error "Implementar"

correcto :: [Prop] -> Prop -> Bool
correcto = error "Implementar"
