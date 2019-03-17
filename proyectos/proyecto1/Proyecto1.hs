-- @autor Rubí Rojas Tania Michelle.
import LogicaProp
import Data.List

-- Las variables proposicionales de las declaraciones de los sospechosos.
p, q, r :: VarP
-- Variable p: Lo hizo el esposo.
p = 'p'
-- Variable q: Lo hizo el amante.
q = 'q'
-- Variable r: Lo hizo el mayordonmo.
r = 'r'

-- Las fórmulas proposicionales de las declaraciones de los sospechosos.
phi, varphi, psi, declaracionFinal :: Prop
-- Proposición phi: Lo que dijo el esposo.
phi = (Conj (Neg (Var p)) (Neg (Var r)))
-- Proposición varphi: Lo que dijo el mayordomo. 
varphi = (Conj (Neg (Var p)) (Var q))
-- Proposición psi: Lo que dijo el amante.
psi = (Conj (Neg (Var q)) (Var p))
-- Declaración final de los sospechosos:
declaracionFinal = (Conj phi (Conj varphi psi))

modelos :: Prop -> [Estado]
modelos phi = [i | i <- estados declaracionFinal, interp phi i == True]

noModelos :: Prop -> [Estado]
noModelos phi = [i | i <- estados declaracionFinal, interp phi i == False]

-- Final del juicio.
especificacion :: Prop -> [Estado]
especificacion declaracion = 
    [i | i <- interseccion (modelos phi) (noModelos varphi) (noModelos psi)]
    ++ [i | i <- interseccion (modelos varphi) (noModelos phi) (noModelos psi)]
    ++ [i | i <- interseccion (modelos psi) (noModelos phi) (noModelos varphi)]

juicio:: Prop -> [Estado]
juicio declaracion =
    [i | i <- especificacion declaracion, 
    (diferentes phi i == True && iguales varphi i == True) && 
    (iguales varphi i == True && iguales psi i == True)
    || (iguales phi i == True && diferentes varphi i == True) && 
    (diferentes varphi i == True && iguales psi i == True)
    || (iguales phi i == True && iguales varphi i == True) && 
    (iguales varphi i == True && diferentes psi i == True)]


interseccion :: Eq a => [a] -> [a] -> [a] -> [a]
interseccion [] [] [] = [] 
interseccion xs ys zs = intersect (intersect xs ys) zs

diferentes :: Prop -> Estado -> Bool
diferentes beta e = 
    (interp (head (varCN beta)) e) /= (interp (last (varCN beta)) e)

iguales :: Prop -> Estado -> Bool
iguales beta e = 
    (interp (head (varCN beta)) e) == (interp (last (varCN beta)) e)