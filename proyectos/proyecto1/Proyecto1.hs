-- @autor Rubí Rojas Tania Michelle.
import LogicaProp
import Data.List

-- Las variables proposicionales de las declaraciones de los sospechosos.
p, q, r :: VarP
-- Variable p = Lo hizo el esposo.
p = 'p'
-- Variable q = Lo hizo el amante.
q = 'q'
-- Variable r = Lo hizo el mayordonmo.
r = 'r'

-- Las fórmulas de las declaraciones de los sospechosos.
desposo, damante, dmayordomo, argumento :: Prop
-- Proposición desposo: Lo que dijo el esposo, es decir, ¬p∧¬r.
desposo = (Conj (Neg (Var p)) (Neg (Var r))) 
-- Proposición damante: Lo que dijo el mayordomo, es decir, ¬p∧q.
dmayordomo = (Conj (Neg (Var p)) (Var q))
-- Proposición psi: Lo que dijo el amante, es decir, ¬q∧p.
damante = (Conj (Neg (Var q)) (Var p))

-- Conjunción de la declaración de los tres sospechosos.
argumento = (Conj desposo (Conj dmayordomo damante))

{- |1. Función unAsesino| Recibe una fórmula (phi). 
-}

unAsesino :: Prop -> [Estado]
unAsesino _ = 
    [e | e <- estados argumento, (buscaBool p e == True && buscaBool q e == False && buscaBool r e == False)
    || (buscaBool p e == False && buscaBool q e == True && buscaBool r e == False)
    || (buscaBool p e == False && buscaBool q e == False && buscaBool r e == True)]

{- |1. Función modelos| Recibe una fórmula (phi). Regresa la lista con todos 
   los modelos que satisfacen a la fórmula (phi).
   Ejemplo: 
   *Main> modelos (Disy (Impl (Var 'p') (Var 'q')) (Neg (Var 'r')))
   [[('p',True),('q',True),('r',True)],[('p',True),('q',True),('r',False)],
   [('p',True),('q',False),('r',False)],[('p',False),('q',True),('r',True)],
   [('p',False),('q',True),('r',False)],[('p',False),('q',False),('r',True)],
   [('p',False),('q',False),('r',False)]]

   /* Notemos que para obtener este resultado, hay que cambiar la parte
      de argumento, por phi. */

   Modificamos esta función para que, en lugar de tomar los estados de la 
   fórmula (phi), tome los estados del argumento. Esto es necesario ya que 
   de lo contrario sólo estaríamos evaluando una declaración con los estados
   posibles de sus dos variables porposicionales, y nosotros necesitamos que
   esté siendo evaluadas con los estados de las tres variables proposicionales.
   Evaluaciones:
   *Main> modelos desposo
   [[('p',False),('r',False),('q',True)],[('p',False),('r',False),('q',False)]]

   *Main> modelos damante
   [[('p',True),('r',True),('q',False)],[('p',True),('r',False),('q',False)]]

   *Main> modelos dmayordomo
   [[('p',False),('r',True),('q',True)],[('p',False),('r',False),('q',True)]]
-}
modelos :: Prop -> [Estado]
modelos phi = 
    [e | e <- estados argumento, interp phi e == True]

{- |2. Función noModelos| Recibe una fórmula (phi). Regresa la lista con todos
   los estados que no satisfacen a la fórmula (phi).
   Ejemplo:
   *Main> noModelos (Disy (Impl (Var 'p') (Var 'q')) (Neg (Var 'r')))
   [[('p',True),('q',False),('r',True)]]

   /* Notemos que para obtener este resultado, hay que cambiar la parte de 
   argumento por phi. */

   Modificamos esta función para que, en lugar de tomar los estados de la
   fórmula (phi), tome los estados del argumento.  Esto es necesario ya que 
   de lo contrario sólo estaríamos evaluando una declaración con los estados
   posibles de sus dos variables porposicionales, y nosotros necesitamos que
   esté siendo evaluadas con los estados de las tres variables proposicionales.
   Evaluaciones de las declaraciones:
   *Main> noModelos desposo
   [[('p',True),('r',True),('q',True)],[('p',True),('r',True),('q',False)],
   [('p',True),('r',False),('q',True)],[('p',True),('r',False),('q',False)],
   [('p',False),('r',True),('q',True)],[('p',False),('r',True),('q',False)]]

   *Main> noModelos damante
   [[('p',True),('r',True),('q',True)],[('p',True),('r',False),('q',True)],
   [('p',False),('r',True),('q',True)],[('p',False),('r',True),('q',False)],
   [('p',False),('r',False),('q',True)],[('p',False),('r',False),('q',False)]]

   *Main> noModelos dmayordomo
   [[('p',True),('r',True),('q',True)],[('p',True),('r',True),('q',False)],
   [('p',True),('r',False),('q',True)],[('p',True),('r',False),('q',False)],
   [('p',False),('r',True),('q',False)],[('p',False),('r',False),('q',False)]]
-}
noModelos :: Prop -> [Estado]
noModelos phi = [e | e <- estados argumento, interp phi e == False]

{- |3. Función unaVerdad| Recibe una fórmula. Regresa la lista de estados
   que satisfacen a una de las declaraciones de los sospechosos y al resto no.
   Expliquemos el por qué tenemos esta función. El problema nos dice que hay 
   una persona que dice dos verdades, por lo que es buena idea comenzar
   descartando todos aquellos estados que no cumplen la propiedad de tener
   sólo una persona que dice la verdad. Es decir, descartamos los estados
   que no satisfacen a ninguna de las declaraciones.
   Esto lo logramos mediante la intersección de los modelos de una declaracion
   y los noModelos de las otras dos declaraciones. Así ganantizamos que 
   obtendremos los estados donde sólo una persona dice la verdad. 
   Por ejemplo, supongamos que el estado 
   [('p', True), ('r', False), ('q', True)] satisface a la declaracion del
   esposo. Como ya tenemos una verdad, los demás tienen que ser no modelos
   de las otras declaraciones pues una persona es un estafador y otra un loco
   que sólo dice una verdad.
   Notemos que no nos importa qué fórmula recibamos como entrada, la función
   siempre nos arrojará el mismo resultado (que es lo que queremos).

   Evaluación de la función:
   *Main> unaVerdad argumento
   [[('p',False),('r',False),('q',False)],[('p',False),('r',True),('q',True)],
   [('p',True),('r',True),('q',False)],[('p',True),('r',False),('q',False)]]
-}
unaVerdad :: Prop -> [Estado]
unaVerdad _ = 
    [i | i <- interseccion (modelos desposo) (noModelos dmayordomo) 
    (noModelos damante)] 
    ++ [i | i <- interseccion (modelos dmayordomo) (noModelos desposo) 
    (noModelos damante)] 
    ++ [i | i <- interseccion (modelos damante) (noModelos desposo) 
    (noModelos dmayordomo)]

{- | 4. Función sonDiferentes| Recibe una fórmula (phi) con únicamente dos
   variables proposicionales y un estado e. Nos dice si la interpretación de
   cada una de sus variables con el estado e son diferentes.
   En esta función es donde 'varCN' cobra mucha importancia. Sabemos que las 
   variables proposicionales de (phi) pueden estar solas o negadas, por lo que
   no nos sirve usar la función 'vars' ya que no estaríamos considerando el 
   caso en que las variables están negadas. Así, utilizamos la función 'varCN' 
   para evaluar ambas variables de la fórmula (phi) y verificar si la 
   interpretación de éstas es diferente o no.
   Ejemplos:
   *Main> sonDiferentes (Conj (Var 'p') (Neg (Var 'q'))) 
    [('p', True), ('q', False)]
   False

   *Main> sonDiferentes (Impl (Var 'r') (Neg (Var 's'))) 
   [('r', True), ('s', False)]
   False

   *Main> sonDiferentes (Syss (Var 'q') (Var 'p')) 
   [('p', False), ('q', True)]
   True
-}
sonDiferentes :: Prop -> Estado -> Bool
sonDiferentes phi e = 
    (interp (head (varCN phi)) e) /= (interp (last (varCN phi)) e)

{- |5. Función sonIguales| Recibe una fórmula (phi) con únicamente dos 
   variables proposicionales y un estado e. Nos dice si la interpretación de
   cada una de sus variables con el estado e son iguales.
   En esta función es donde 'varCN' cobra mucha importancia. Sabemos que las 
   variables proposicionales de (phi) pueden estar solas o negadas, por lo que
   no nos sirve usar la función 'vars' ya que no estaríamos considerando el 
   caso en que las variables están negadas. Así, utilizamos la función 'varCN' 
   para evaluar ambas variables de la fórmula (phi) y verificar si la 
   interpretación de éstas es igual o no.
   Ejemplos:
   *Main> sonIguales (Conj (Var 'p') (Neg (Var 'q'))) 
   [('p', True), ('q', False)]
   True

   *Main> sonIguales (Impl (Var 'r') (Neg (Var 's'))) 
   [('r', True), ('s', False)]
   True

   *Main> sonIguales (Syss (Var 'q') (Var 'p')) 
   [('p', False), ('q', True)]
   False
-}
sonIguales :: Prop -> Estado -> Bool
sonIguales beta e = 
    (interp (head (varCN beta)) e) == (interp (last (varCN beta)) e)

{- |6. Función juicio| Recibe una fórmula. Regresa la lista con los
   estados que cumplen la propiedad de que exista un lógico (dos verdades),
   un estafador (dos mentiras) y un loco (una verdad y una mentira).
   Expliquemos cómo funciona juicio. Tomamos los estado que obtuvimos de la
   función 'unaVerdad', los cuales son cuatro. Y como sabemos que en éstos 
   estados sólo una persona dice la verdad, entonces debemos buscar al estafador
   y al loco, los cuales tienen una característica muy peculiar: la 
   interpretación de cada una de las variables proposicionales de la 
   declaración de éstos dos personajes con algún estado de la función 
   'unaVerdad' es diferente (si es el loco) e igual (si es el estafador).
   Entonces, recordemos que sabemos que hay un lógico, el cual dice dos
   verdades y por lo tanto la interpretación de cada una de sus variables 
   es igual con algún estado de 'esVerdad'. Sabiendo esto, basta considerar 
   tres casos simples para deducir quién es quién. Veremos sólo el primer caso,
   ya que los demás son análogos.
   Supongamos que la interpretación de las variables de la declaracion del
   esposo con algún estado de 'unaVerdad' es diferente y la interpretación
   de las variables de la declaración del mayordomo con algún estado de 
   'unaVerdad' es igual. Entonces sabemos que el esposo es el loco (ya que 
   dice una verdad y una mentira) y el mayordomo puede ser el estafador o el
   lógico. Pero si además, la interpretación de las variables proposicionales
   de la declaración del amante con algún estado de 'unaVerdad' son iguales
   entonces significa que el amante también puede ser el estafador y el lógico.
   Y como ese "alǵun estado de 'unaVerdad'", es el mismo que aplicamos en las 
   tres declaraciones, entonces sabemos que ese estado es el que cumple la
   propiedad que estamos buscando. Bastaría conseguir el estado y revisar 
   manualmente quién es nuestro lógico y nuestro estafador.
   
   Evaluación de la función:
   *Main> juicio argumento
   [[('p',False),('r',True),('q',True)],[('p',True),('r',False),('q',False)]]

-}
juicio:: Prop -> [Estado]
juicio _ = 
    [e | e <- unaVerdad argumento, 
    (sonDiferentes desposo e == True && sonIguales dmayordomo e == True) 
    && (sonIguales dmayordomo e == True && sonIguales damante e == True)
    || (sonIguales desposo e == True && sonDiferentes dmayordomo e == True) 
    && (sonDiferentes dmayordomo e == True && sonIguales damante e == True)
    || (sonIguales desposo e == True && sonIguales dmayordomo e == True) 
    && (sonIguales dmayordomo e == True && sonDiferentes damante e == True)]

-- Funciones auxiliares. --

{- |1. Función auxiliar intersección| Recibe tres listas xs, ys, zs. 
   Regresa una lista con los elementos que tienen en común las tres listas.
   Para nuestro ca--so particular, la utilizamos para obtener la intersección
   de tres listas de listas. Notemos que aquí utilizamos la función intersect,
   nos ahorra tener que hacer a pie la función de intersección. Nuestra función
   simplemente es un caso particular que necesitamos para el proyecto.
   Ejemplos:
   *Main> interseccion [1,2,3] [3,4,5] [5,3,7]
   [3]

   *Main> interseccion [[1,2,3], [4,5,6]] [[4,5,7], [1,2,3]] [[1,2,4], [1,2,3]]
   [[1,2,3]]
-}
interseccion :: Eq a => [a] -> [a] -> [a] -> [a]
interseccion [] [] [] = [] 
interseccion xs ys zs = intersect (intersect xs ys) zs