-- Tipo de dato para representar expresiones de la lógica proposicional.
data Prop = Top | Bot | Var String | Neg Prop | Conj Prop Prop | Disy Prop Prop 
            | Impl Prop Prop | Syss Prop Prop deriving Eq

type Estado = [(String, Prop)]

-- Proposición p: Lo hizo el esposo.
-- Proposición q: Lo Hizo el amante.
-- Proposición r: Lo hizo el mayordonmo.

-- Proposición phi: Lo que dijo el esposo.
phi = (Conj (Neg (Var "P")) (Neg (Var "R"))) 
varphi = (Conj (Neg (Var "P")) (Var "Q"))
psi = (Conj (Neg (Var "Q")) (P))