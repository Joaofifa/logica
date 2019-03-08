-- Tipo de dato para representar expresiones de la lógica proposicional.
data Prop = Top | Bot | Var String | Neg Prop | Conj Prop Prop | Disy Prop Prop 
            | Impl Prop Prop | Syss Prop Prop deriving (Eq, Ord)

type Estado = [(String, Bool)]

{- l
-}


{- Recibe una expresión proposicional y un estado. Regresa la interpretación
   de la expresión proposicional con el estado dado. 
-}
interp :: Prop -> Estado -> Bool
interp phi e = case phi of 
    Top -> True
    Bot -> False 
    Var i -> lookUp i e
    Neg p -> not (interp p e)
    Conj p q -> interp p e && interp q e
    Disy p q -> interp p e || interp q e
    Impl p q -> not (interp p e) || interp q e
    Syss p q -> interp p e == interp q e

{- Recibe una fórmula (phi) con n-variables proposicionales. Regresa la lista
   con los 2^n estados distintos para (phi).

estados:: Prop -> [Estado]
estados [] = [[]]
estados (x:xs) = 
    [(x,False) | x <- subconj (vars phi)] ++ [(x, True) | x <- subconj (vars phi)]
-}
    
{- Recibe una fórmula (phi). Regresa la lista de variables proposicionales que
   figuran en (phi), sin repetición.
-}
vars :: Prop -> [Prop]
vars phi = case phi of
    Top -> []
    Bot -> []
    Var p -> [Var p]
    Neg p -> vars p
    Conj p q -> vars p ++ vars q
    Disy p q -> vars p ++ vars q
    Impl p q -> vars p ++ vars q
    Syss p q -> vars p ++ vars q

{- Recibe una lista A. Regresa la lista con las sublistas de A.
-}
subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = sub ++ map (x:) sub
   where sub = subconj xs