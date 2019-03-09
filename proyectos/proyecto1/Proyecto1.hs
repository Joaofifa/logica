import LogicaProp

-- Las variables proposicionales de las declaraciones de los sospechosos.
p, q, r :: VarP
-- Variable p: Lo hizo el esposo.
p = 'p'
-- Variable q: Lo hizo el amante.
q = 'q'
-- Variable r: Lo hizo el mayordonmo.
r = 'r'

-- Las fórmulas proposicionales de las declaraciones de los sospechosos.
phi, varphi, psi :: Prop
-- Proposición phi: Lo que dijo el esposo.
phi = (Conj (Neg (Var p)) (Neg (Var r)))
-- Proposición varphi: Lo que dijo el mayordomo. 
varphi = (Conj (Neg (Var p)) (Var q))
-- Proposición psi: Lo que dijo el amante.
psi = (Conj (Neg (Var q)) (Var p))

-- Final del juicio.
