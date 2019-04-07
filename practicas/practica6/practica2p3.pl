/**
 * |1| Regla range. Recibe tres parámetros: M, N y L.
 * La regla se satisface si L contiene el rango entre M y N.
 *
 * Descripción.
 * (a) Si M = N entonces el rango entre M y N es la lista que contiene a M.
 * (b) Regla recursiva: Tenemos dos posibles casos
 * 1. Si M < N, entonces M es la cabeza de la lista L y hacemos recursión sobre
 * el siguiente número de M. Así obtenemos una lista de la forma 
 * [M, M+1, ..., N].
 * 2. Si N < M, entonces M es la cabeza de la lista L y hacemos recursión sobre
 * el anterior número a M. Así obtenemos una lista de la forma
 * [M, M-1, ..., N].
 * 
 * Ejemplos de entrada:
 * ?- range(1,9, L).
   L = [1, 2, 3, 4, 5, 6, 7, 8, 9] ;
   false.

 * ?- range(9, 1, L).
   L = [9, 8, 7, 6, 5, 4, 3, 2, 1] ;
   false.

 * ?- range(1,2, [1,2]).
   true ;
   false.

 */
range(M, M, [M]).
range(M, N, [M|ZS]) :- M<N -> K is M+1, range(K, N, ZS).
range(M, N, [M|ZS]) :- N<M -> K is M-1, range(K, N, ZS).

/**
 * |2| Regla rotate. Recibe tres parámetros: L, N y A.
 * La regla se satisface si se rotan los primeros N elementos de la lista L.
 * La lista resultante se guarda en la variable A.
 *
 * Descripción.
 * (a) Si N=0 entonces no se rota la lista.
 * (b) Regla recursiva: Cuando nos encontramos con una lista L que tiene al 
 * menos un elemento entonces sabemos que al menos una posición es la que 
 * vamos a rotar, por lo que concatenamos la cola de la lista L con la lista
 * que contiene la cabeza de L, es decir, nos queda [xs, x]. El resultado lo
 * guardamos en la variable R. Finalmente hacemos recursión sobre la lista 
 * R y la posición N-1 para que así podemos ir concatenando y guardando las
 * cabezas de R después de su cola en la lista A.
 *
 * Ejemplos de entrada:
 * ?- rotate([3,1,5,1,2,1,7,1,9], 7, A).
   A = [1, 9, 3, 1, 5, 1, 2, 1, 7].

 * ?- rotate([s,a,r,a,t,o,g,a], 1, A).
   A = [a, r, a, t, o, g, a, s].

 * ?- rotate([q,u,e,d,a,t,e], 3, [d,a,t,e,q,u,e]).
   true.

 */
rotate(L, 0, L) :- !.
rotate([X|XS], N, A) :- N>0, K is N-1, concatena(XS, [X], R), rotate(R, K, A).

/**
 * |3| Regla compress. Recibe dos parámetros: L y Lc.
 * La regla se satisface si Lc tiene la compresión de los elementos de la 
 * lista L.
 *
 * Descripción.
 * (a) Si ambas listas son vacías ó tienen un sólo elemento y es el mismo, 
 * entonces es cierto que una es la compresión de la otra. 
 * (b) Regla recursiva: Cuando nos encontramos con una lista que inicia con
 * dos elementos consecutivos repetidos, entonces hacemos recursión sobre la
 * lista que tiene sólo una aparición de ese elemento. Cuando nos encontramos
 * con una lista cuyos dos primeros elementos son diferentes, la cabeza de la 
 * lista Lc es el primer elemento de L y hacemos recursión sobre la lista
 * que contiene al segundo elemento de L como la cabeza y la cola de L.
 *
 * Ejemplos de entrada:
 * compress([N,o,o,o,o,o,m,e,e,e,e,e,e,g,u,u,s,s,s,t,a,a,a,a,a,a,a,a], LC).
   LC = [N, o, m, e, g, u, s, t, a].

 * compress([a,a,a,a,m,m,a,m,m,m,m,m,m,e], LC).
   LC = [a, m, a, m, e].

 * compress([D,D,D,o,o,v,v,v,v,v,v,v,e,e,e,r,r,r,e,e,s,s,s,s,t,t,o,o,o], LC).
   LC = [D, o, v, e, r, e, s, t, o].

 */
compress([],[]).
compress([X],[X]).
compress([X, Y|YS], [X|ZS]) :- compress([Y|YS], ZS), X \== Y, !.
compress([X, X|XS], LC) :- compress([X|XS], LC).

/**
 * |9| Regla preorder. Recibe dos parámetros: T y P.
 * La regla se satisface si P es el recorrido en pre-orden en el árbol T.
 *
 * Descripción.
 * (a) Si el árbol es vacío, entonces su recorrido en pre-orden es la lista
 * vacía.. Si el árbol tiene únicamente un elemento, entonces su recorrido
 * en pre-orden es la lista que contiene a ese elemento. 
 * (b) Regla recursiva: Para realizar el recorrido en pre-orden hay que 
 * hacer recursivamente las siguientes operaciones en cada nodo, 
 * iniciando con la raíz:
 * 1. Visitamos la raíz.
 * 2. Visitamos el sub-árbol izquierdo.
 * 3. Visitamos el sub-árbol derecho.
 * Notemos que la regla debe ser ejecutada con la raíz del árbol inicialmente.
 * Como se procesa el vértice actual antes de hacer la recursión, la raíz de 
 * cada subárbol se imprime antes que todo el resto del subárbol. Entonces 
 * para la regla lo único que tenemos que hacer es hacer recursión sobre el 
 * subárbol izquierdo y el subárbol derecho mientras concatenamos la raíz del
 * árbol con las raíces de los subárboles que vamos visitando.
 *
 * Ejemplos de entrada:
 * ?- preorder(t(1, t(9, nil, nil), t(12, nil, nil)), P).
   P = [1, 9, 12].

 * ?- preorder(t(12, t(2, nil, nil), t(8, t(7, nil, nil), t(0, nil, nil))), P).
   P = [12, 2, 8, 7, 0].
 
 * ?- preorder(t(8, t(4, t(2, t(1, nil, nil), t(3, nil, nil)), 
   t(6, t(5, nil, nil), t(7, nil, nil))), t(12, t(10, t(9, nil, nil), 
   t(11, nil, nil)), t(14, t(13, nil, nil), t(15, nil, nil)))), 
   [8,4,2,1,3,6,5,7,12,10,9,11,14,13,15]).
   true.
  
 */
preorder(t(nil,nil,nil), []) :- !.
preorder(t(E, nil, nil), [E]) :- !.
preorder(t(E, TI, TD), P) :- 
  preorder(TI, R1), preorder(TD, R2), concatena([E|R1], R2, P).

/* --- Reglas auxiliares. --- */

/**
 * |Aux. 1| Regla concatena. Recibe tres parámetros: L1, L2 y LF.
 * La regla se satisface si L1 se concatena con L2, obteniendo la lista LF.
 *
 * Descripción.
 * (a) Si L1 es la lista vacía, entonces la concatenación de L1 y L2 es L2.
 * (b) Regla recursiva: La concatenación de L1 y L2 es la lista LF, cuyo 
 * primer elemento es la cabeza de L1 y cuya cola es la concatenación de
 * la cola de L1 y L2.
 * 
 * Ejemplos de entrada:
 * ?- concatena([3,1,5], B, [3,1,5,1,2,1,7,1,9]).
   B = [1, 2, 1, 7, 1, 9].

 * ?- concatena([3,2,9], [4,0,6], LF).
   LF = [3, 2, 9, 4, 0, 6].

 * ?- concatena(['A','s','t','a'], ['r','o','t','h'], LF).
   LF = ['A', s, t, a, r, o, t, h].
 
 */
concatena([], L2, L2).
concatena([X|XS], L2, [X|YS]) :- concatena(XS, L2, YS).