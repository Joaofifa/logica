module Lab4 where

import Data.Char

iniciooracion :: String -> String
iniciooracion [] = []
iniciooracion (x:xs) = toUpper x:[toLower x|x <- xs] 

inversion :: [a] -> [a]
inversion = error "Implementar"

junta :: Eq a => [[a]] -> [[a]]
junta = error "Implementar"

palindrome :: (Eq a) => [a] -> Bool
palindrome = error "Implementar"

-- Invierte una lista usando foldr
inversionL :: [a] -> [a]
inversionL = error "Implementar"

data Graph = Graph { gId :: Int, 
					nodes :: [String],
					edges :: [(String, String)]
					} deriving (Show, Eq)

neighbors :: String -> Graph -> [String]
neighbors = error "Implementar"

-- Tipo de dato para representar expresiones de la lógica proposicional
data Prop = TTrue
           | FFalse 
           | Var String
           | Neg Prop
           | Conj Prop Prop -- (P ∧ Q)
           | Disy Prop Prop -- (P ∨ Q)
           | Impl Prop Prop -- (P → Q)
           | Syss Prop Prop -- (P ↔ Q)
           deriving (Eq,Ord)

-- Sinónimo para representar el estado
-- Se tiene Var "Q"
-- Es estado [("Q", TTrue)]
type Estado = [(String, Prop)]

--instance Show Prop where 