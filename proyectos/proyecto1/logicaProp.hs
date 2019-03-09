-- Las variables proposicionales son del tipo Char.
type VarP = Char

-- El tipo de dato de las fórmulas proposicionales.
data Prop = Top | Bot | Var VarP | Neg Prop | Conj Prop Prop | Disy Prop Prop 
            | Impl Prop Prop | Syss Prop Prop 
            deriving (Eq, Ord, Show)

{- Los estados son listas de tuplas tal que su primer componente es el nombre
   de una variable proposicional y su segundo componente es un valor booleano.
-}
type Estado = [(VarP, Bool)]

{- Recibe una expresión proposicional (phi) y un estado e. Regresa la 
   interpretación de la expresión proposicional con el estado dado. 
-}
interp :: Prop -> Estado -> Bool
interp phi e = case phi of 
    Top -> True
    Bot -> False 
    Var i -> busca i e
    Neg p -> not (interp p e)
    Conj p q -> interp p e && interp q e
    Disy p q -> interp p e || interp q e
    Impl p q -> not (interp p e) || interp q e
    Syss p q -> interp p e == interp q e

{- Regresa la lista de variables proposicionales que figuran en (phi), 
   sin repetición.
-}
vars :: Prop -> [VarP]
vars phi = case phi of
    Top -> []
    Bot -> []
    Var i-> [i]
    Neg p -> vars p
    Conj p q -> vars p ++ vars q
    Disy p q -> vars p ++ vars q
    Impl p q -> vars p ++ vars q
    Syss p q -> vars p ++ vars q

{- Recibe una fórmula (phi) con n-variables proposicionales. Regresa la lista
   con los 2^n estados distintos para (phi).
-}
estados :: Prop -> [Estado]
estados phi = subconj (vars phi)
   where 
    subconj [] = [[]]
    subconj (x:xs) = 
        [(x, True) : i | i <- subconj xs] ++ [(x, False) : i | i <- subconj xs] 

{- Recibe una fórmula proposicional (phi). Regresa la lista con todos aquellos
   estados I tales que I(phi) = 1.
-}
modelos:: Prop -> [Estado]
modelos phi = [i | i <- estados phi, interp phi i == True]

-- Nos dice si una fórmula proposicional (phi) es una tautología.
tautologia :: Prop -> Bool 
tautologia phi = estados phi == modelos phi

{- Nos dice si una fórmula proposicional (phi) es satisfacible en una 
   interpretación.
-}
satisfen :: Prop -> Estado -> Bool 
satisfen phi e = interp phi e == True

-- Nos dice si una fórmula proposicional (phi) es satisfacible.
satisf :: Prop -> Bool
satisf phi = modelos phi /= []

-- Nos dice si una fórmula proposicional (phi) es insatisfacible.
insatisfen :: Prop -> Estado -> Bool 
insatisfen phi e = interp phi e == False

-- Nos dice si una fórmula proposicional (phi) es una contradicción.
contrad :: Prop -> Bool 
contrad phi = modelos phi == []

-- Funciones auxiliares.

{- Recibe una variable proposicional. 
-}
busca :: (Eq c) => c -> [(c,b)] -> b 
busca c t = head [b | (x,b) <- t, c == x]
