-- @autor Rubí Rojas Tania Michelle.
module Proyecto1 where 
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

{- |1. Función unAsesino| Recibe una fórmula. Regresa los únicos tres estados
   donde se cumple que sólo hay un asesino. 

   El problema nos dice que sólo hay un asesino, y de acuerdo a cómo definimos
   nuestras variables proposicionales p, q y r, entonces en los estados donde 
   se cumple que sólo hay un asesino pasa que p es verdadero y los demás son 
   falsos, ó q es verdadero y los demás falsos, ó r es verdadero y los demás
   son falsos. Entonces esta función es muy importante pues así sólo estaremos 
   trabajando en los estados que cumplen esta primera condición del problema.

   Para lograr esto, utilizamos una lista de comprensión para obtener la lista 
   con los tres estados que estamos buscando. Los estados los obtenemos de los 
   2^3 = 8 estados posibles de nuestro argumento. Las propiedades en la lista 
   de comprensión usan la función auxiliar buscaBool para definir cómo son
   los valores booleanos de cada una de las variables proposicionales de los
   estados que estamos buscando. Así garantizamos que buscamos los estados 
   correctos.

   Notemos que no nos importa qué fórmula recibamos como entrada (debe ser
   una fórmula válida para que haskell no llore, así que utilizamos 
   argumento por simplicidad), la función siempre nos arrojará el mismo 
   resultado (que es lo que queremos).

   Salida de la función:
   *Main> unAsesino argumento
   [[('p',True),('r',False),('q',False)],[('p',False),('r',True),('q',False)],
   [('p',False),('r',False),('q',True)]]
-}
unAsesino :: Prop -> [Estado]
unAsesino _ = 
    [e | e <- estados argumento, 
    (buscaBool p e == True && buscaBool q e == False && buscaBool r e == False)
    || (buscaBool p e == False && buscaBool q e == True && buscaBool r e == False)
    || (buscaBool p e == False && buscaBool q e == False && buscaBool r e == True)]

{- |2. Función modelos| Recibe una fórmula (phi). Regresa la lista con todos 
   los modelos de la fórmula (phi).

   Para obtener los modelos utilizamos una lista de comprensión, la cual tiene
   como propiedad que la interpretación de (phi) con los estados de unAsesino
   sean igual a True (y como vimos en la parte teórica, esto nos dice que si
   se cumple la propiedad descrita anteriormente, entonces ese es un modelo de
   phi). Notemos que en lugar de tomar los estados de la fórmula (phi), tomamos
   los estados obtenidos de la función unAsesino con el argumento. ¿Por qué? 
   Bueno, esta función unicamente la usaremos para obtener los modelos de cada 
   una de las declaraciones de los sospechosos, y como los únicos estados que
   nos interesan son los de unAsesino, entonces utilizamos éstos. 

   Esta función será de mucha utilidad en unaVerdad ya que ahí empezamos a 
   descartar estados que no cumplan la propiedad de que sólo una persona dice
   la verdad. Los estados que queden implican que existe al menos el
   personaje lógico.
   
   Salida de la función:
   *Main> modelos desposo
   [[('p',False),('r',False),('q',True)]]

   *Main> modelos dmayordomo
   [[('p',False),('r',False),('q',True)]]

   *Main> modelos damante
   [[('p',True),('r',False),('q',False)]]
-}
modelos :: Prop -> [Estado]
modelos phi = [e | e <- unAsesino argumento, interp phi e == True]

{- |3. Función noModelos| Recibe una fórmula (phi). Regresa la lista con todos
   los estados que no satisfacen a la fórmula (phi).

   Para obtener los no modelos utilizamos una lista de comprensión, la cual 
   tiene como propiedad que la interpretación de (phi) con los estados de 
   unAsesino sean igual a False (y como vimos en la parte teórica, esto nos 
   dice que si se cumple la propiedad descrita anteriormente, entonces ese no
   es un modelo de phi). Notemos que en lugar de tomar los estados de la 
   fórmula (phi), tomamos los estados obtenidos de la función unAsesino con 
   el argumento. ¿Por qué? Bueno, esta función unicamente la usaremos para 
   obtener los no modelos de cada una de las declaraciones de los sospechosos, 
   y como los únicos estados que nos interesan son los de unAsesino, 
   entonces utilizamos éstos. 

   Esta función será de mucha utilidad en unaVerdad ya que ahí empezamos a 
   descartar estados que no cumplan la propiedad de que sólo una persona dice
   la verdad. Los estados que queden implican que existe al menos el
   personaje lógico.

   Salida de la función:
   *Main> noModelos desposo
   [[('p',True),('r',False),('q',False)],[('p',False),('r',True),('q',False)]]

   *Main> noModelos dmayordomo
   [[('p',True),('r',False),('q',False)],[('p',False),('r',True),('q',False)]]

   *Main> noModelos damante
   [[('p',False),('r',True),('q',False)],[('p',False),('r',False),('q',True)]]
-}
noModelos :: Prop -> [Estado]
noModelos phi = [e | e <- unAsesino argumento, interp phi e == False]

{- |4. Función unaVerdad| Recibe una fórmula. Regresa la lista de estados que
   satisfacen a una de las declaraciones de los sospechosos y al resto no.

   El problema nos dice que hay una persona que dice dos verdades, por lo que 
   es buena idea comenzar descartando todos aquellos estados que no cumplen 
   la propiedad de tener sólo una persona que dice la verdad (ya que de lo
   contrario no se cumple que exista un lógico, un estadafor y un loco). 
   Esto lo logramos mediante la intersección de los modelos de una declaracion
   y los noModelos de las otras dos declaraciones. Así, nuestra lista de 
   comprensión tiene como propiedad todos los posibles casos de la condición
   descrita anteriormente.

   Por ejemplo, sabemos que el estado [('p',False),('r',False),('q',True)]
   satisface a la declaracion del esposo. Gracias a esto ya tenemos al 
   personaje lógico (ya que si lo satisface, entonces dice dos verdades pues
   el conectivo de las declaraciones es la conjunción). Entonces, este estado
   no tendría que ser un modelo para la declaración del mayordomo y del
   amante. Si este estado cumple lo anterior, entonces cumple la propiedad
   de que al menos existe un lógico y ya sólo tendríamos que comprobar la 
   existencia del loco y del estafador.

   Notemos que no nos importa qué fórmula recibamos como entrada (debe ser
   una fórmula válida para que haskell no llore, así que utilizamos 
   argumento por simplicidad), la función siempre nos arrojará el mismo 
   resultado (que es lo que queremos).

   Salida de la función:
   *Main> unaVerdad argumento
   [[('p',True),('r',False),('q',False)]]
-}
unaVerdad :: Prop -> [Estado]
unaVerdad _ = 
    [i | i <- interseccion (modelos desposo) (noModelos dmayordomo) 
    (noModelos damante)] 
    ++ [i | i <- interseccion (modelos dmayordomo) (noModelos desposo) 
    (noModelos damante)] 
    ++ [i | i <- interseccion (modelos damante) (noModelos desposo) 
    (noModelos dmayordomo)]

{- |5. Función juicio| Recibe una fórmula. Regresa la lista con los
   estados que cumplen la propiedad de que exista un lógico (dos verdades),
   un estafador (dos mentiras) y un loco (una verdad y una mentira).

   Expliquemos cómo funciona juicio. Tomamos los estado que obtuvimos de la
   función 'unaVerdad', el cuál sólo es uno (pero aún así hay que verificar
   que ese estado cumple con las propriedades del problema). Y como sabemos 
   que en este estado sólo una persona dice la verdad, entonces debemos buscar 
   al estafador y al loco, los cuales tienen una característica muy peculiar: 
   la  interpretación de cada una de las variables proposicionales de la 
   declaración de éstos dos personajes con el estado de la función 
   'unaVerdad' debería ser diferente (si es el loco) e igual (si es el 
   estafador). Entonces, recordemos que sabemos que hay un lógico, el cual 
   dice dos verdades y por lo tanto la interpretación de cada una de sus 
   variables es igual (por ser conjunción) con el estado de 'esVerdad'. 
   Sabiendo esto, basta considerar tres casos simples en nuestra lista de 
   comprensión para deducir que existen los tres personajes solicitados. 
   Veremos sólo el primer caso, ya que los demás son análogos.

   Supongamos que la interpretación de las variables de la declaracion del
   esposo con el estado de 'unaVerdad' es diferente y la interpretación
   de las variables de la declaración del mayordomo con algún estado de 
   'unaVerdad' es igual. Entonces sabemos que el esposo es el loco (ya que 
   dice una verdad y una mentira) y el mayordomo puede ser el estafador o el
   lógico. Pero si además, la interpretación de las variables proposicionales
   de la declaración del amante con el estado de 'unaVerdad' son iguales
   entonces significa que el amante también puede ser el estafador ó el lógico.
   Y como ese estado de 'unaVerdad', es el mismo que aplicamos en las 
   tres declaraciones, entonces sabemos que ese estado es el que cumple la
   propiedad que estamos buscando. Bastaría revisar manualmente quién es 
   nuestro lógico y nuestro estafador.

   Notemos que no nos importa qué fórmula recibamos como entrada (debe ser
   una fórmula válida para que haskell no llore, así que utilizamos 
   argumento por simplicidad), la función siempre nos arrojará el mismo 
   resultado (que es lo que queremos).
   
   Evaluación de la función:
   *Main> juicio argumento
   [[('p',True),('r',False),('q',False)]]
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

   Para nuestro caso particular, la utilizamos para obtener la intersección
   de tres listas de listas en la función 'unaVerdad'. Notemos que aquí 
   utilizamos la función intersect, ya que nos ahorra tener que hacer a pie 
   la función de intersección. Nuestra función simplemente es un caso 
   particular que necesitamos para el proyecto.

   Ejemplos:
   *Main> interseccion [1,2,3] [3,4,5] [5,3,7]
   [3]

   *Main> interseccion [[1,2,3], [4,5,6]] [[4,5,7], [1,2,3]] [[1,2,4], [1,2,3]]
   [[1,2,3]]
-}
interseccion :: Eq a => [a] -> [a] -> [a] -> [a]
interseccion [] [] [] = [] 
interseccion xs ys zs = intersect (intersect xs ys) zs