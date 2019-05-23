/** 
 * Integrantes del equipo:
 * 1. 
 * 2. Rubí Rojas Tania Michelle.
 */

/* Las casillas son el universo del discurso. */
casilla(0,1).
casilla(1,1).
casilla(0,2).
casilla(2,0).
casilla(2,1).
casilla(2,2).
casilla(0,4).
casilla(0,5).
casilla(1,5).
casilla(2,5).
casilla(3,5).
casilla(4,5).
casilla(5,5).
casilla(3,2).
casilla(3,3).
casilla(3,4).
casilla(5,3).
casilla(5,4).
casilla(0,4).
casilla(0,5).

/**
 * |1| Regla conexion. Recibe dos parámetros: A y B.
 * La regla se satisface si la casilla A está conectada con la casilla B.
 *
 * Descripción: 
 * (i) El conjunto de conocimiento (tablero) está representado con la regla 
 * conexion(A, B). Simplemente indicamos explícitamente cuáles casillas están
 * conectadas entre sí.
 * (ii) Sabemos que la relación "a está conectado con b" es de equivalencia, 
 * por lo que añadimos la simetría en la regla conexión, la cual nos será de 
 * mucha ayuda al momento de buscar el camino deseado.
 *
 */
conexion(casilla(0,1),casilla(1,1)).
conexion(casilla(0,1),casilla(0,2)).
conexion(casilla(1,1),casilla(2,1)).
conexion(casilla(2,0),casilla(2,1)).
conexion(casilla(2,1),casilla(2,2)).
conexion(casilla(2,2),casilla(3,2)).
conexion(casilla(3,2),casilla(3,3)).
conexion(casilla(3,3),casilla(3,4)).
conexion(casilla(3,4),casilla(2,4)).
conexion(casilla(3,4),casilla(3,5)).
conexion(casilla(3,5),casilla(2,5)).
conexion(casilla(2,4),casilla(2,5)).
conexion(casilla(2,5),casilla(1,5)).
conexion(casilla(1,5),casilla(0,5)).
conexion(casilla(0,5),casilla(0,4)).
conexion(casilla(3,5),casilla(4,5)).
conexion(casilla(4,5),casilla(5,5)).
conexion(casilla(5,5),casilla(5,4)).
conexion(casilla(5,4),casilla(5,3)).
conexion(casilla(5,0),casilla(4,0)).
conexion(casilla(4,0),casilla(4,1)).
% Simetría.
conexion(A, B) :- casilla(A, B).
conexion(A, B) :- casilla(B, A).

/**
 * |2| Regla busca. Recibe tres parámetros: I, F y C.
 * La regla se satisface si C es un camino que va de la casilla inicial I a la
 * casilla final F.
 *
 * Descripción:
 * (i) Si la casilla de inicio es la misma que la casilla final, entonces 
 * el camino es la lista vacía (ya que no hay que recorrer nada para llegar
 * a la misma casilla).
 * (ii) Regla recursiva: Utilizamos una regla auxiliar llamada dfs para 
 * poder buscar el camino (si es que existe) que va de la casilla inicial I a la
 * casilla final F.
 *
 * Ejemplos de entrada:
 * -------- Tres ejemplos ---------------
 * 
 */
%busca(I, I, []).
busca(I, I, []).
busca(I, F, C) :- dfs(I, F, [I], P), reverse(P, C).

/**
 * |3| Regla dfs. Recibe cuatro parámetros: I, F, V, C.
 * La regla se satisface si C es el camino que va de la casilla inicial I y la 
 * casilla final F, y V son las casillas visitadas al buscar C.
 *
 * Descripción:
 * (i) Si C es la lista que contiene a la casilla I y a las casillas visitadas V,
 * entonces obtenemos la conexión entre I y F.
 * (ii) Regla recursiva: Obtenemos a un vecino de la casilla I, verificamos que 
 * ese vecino sea diferente de la casilla F, verificamos que ese vecino no esté
 * en la lista de casillas visitadas y hacemos recursión sobre el vecino, la 
 * casilla F, la lista que contiene al veicno y a la lista de casillas ya 
 * visitadas, y el camino C.
 *
 */
dfs(I, F, V, [I|V]) :- conexion(I, F).
dfs(I, F, V, C) :- conexion(I, B), B \== F, \+ (member(B, V)), dfs(B, F, [B|V], C).