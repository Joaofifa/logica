persona(juan).

capital(usa, washington).
capital(uk, london).
capital(colombia, bogota).
capital(japon, tokio).
capital(usa, ny).
capital(mexico, ciudaddemexico).
capital(cabo_verde, praia).

paises(X,Y) :- persona(X), ingles(Y).
viajar(X,Z) :- paises(X,Y), capital(Y,Z).

moneda(uk, euros).
moneda(colombia, pesos).
moneda(mexico, pesos).
moneda(usa, dolar).
moneda(japon, yen).

lengua(usa, ingles).
lengua(uk, ingles).
lengua(mexico, espagnol).
lengua(colombia, espagnol).
lengua(japon, japones).

% | 1 | Regla quevisitar. Recibe dos parámetros: C y L. C regresa las capitales
% que podemos viistar hablando la lengua L. 
quevisitar(C,L) :- capital(A,C), lengua(A,L).

% | 2 | Regla quevisitar2. Recibe tres parámetros. C regresa las capitales que
% podemos visitar hablando la lengua L y pagando con la moneda M.
quevisitar2(C,L,M) :- capital(A,C), lengua(A,L), moneda(A,M).

%-------------------------
%     Árbol genalógico
%         SIMPSON
%-------------------------
papa(homero, maggie).
papa(homero, lisa).
papa(homero, bart).
papa(abraham, homero).
papa(abraham, herbert).
papa(abraham, abbie).
papa(clancy, marge).
papa(clancy, selma).
papa(clancy, patty).
mama(marge, maggie).
mama(marge, lisa).
mama(marge, bart).
mama(mona, homero).
mama(edwina, abbie).
mama(???,herbert).
mama(jacqueline, marge).
mama(jacqueline, selma).
mama(jacqueline, patty).
mama(selma, ling).
%-------------------------

hijo(Y,X) :- papa(X,Y); mama(X,Y). 
% Claramente Y es hijo de X si, X es papa de Y ó si X es mamá de Y.

% | 3 | Regla padres. Recibe tres parámetros: A, B y C. La regla es 
% satisfactoria cuando A y B son los papás de C.
padres(A,B,C) :- papa(A,C), mama(B,C).  

% | 4 | Regla hermano. Recibe dos parámetros: M y N. La regla es 
% satisfactoria cuando M es hermano de N. 
hermano(M,N) :- padres(A,B,M), padres(A,B,N), M \== N.

% | 5 | Regla mediohermano. Recibe dos parámetros: D y E. La regla es 
% satisfactoria cuando D es mediohermano de E.
mediohermano(D,E) :- 
    ((papa(A,D), papa(A,E)); (mama(A,D), mama(A,E))), not(hermano(D,E)), 
    D \== E.

% | 6 | Regla tio. Recibe dos parámetros: S y T. La regla es 
% satisfactoria cuando S es tío de T. 
tio(S,T) :- (hermano(S,A); mediohermano(S,A)), (mama(A,T); papa(A,T)), S \== A.

% | 7 | Regla hss. Recibe dos parametros: A y B. La regla sera satisfactoria
% cuando se cumpla la propiedad de que A es hermano o mediohermano de B.
hss(A,B) :- (hermano(A,B); mediohermano(A,B)), A \== B.

% | 8 | Regla primo. Recibe dos parametros: U y V. La regla sera satisfactoria
% cuando se cumpla la propiedad de que U es primo de V.
primo(U,V) :- 
    padres(A,B,U), padres(C,D,V), (hermano(A,C); hermano(A,D); hermano(B,C); 
    hermano(B,D)), U \== V.

% | 9 | Regla tutor. Recibe dos parametros: A y B. La regla se cumple cuando
% se cumpla la propiedad de que A es tutor de B.
tutor(A,B) :- papa(A,B); mama(A,B), A \== B.

% | 10 | Regla abuelo. Recibe dos parametros: C y D. La regla sera
% satisfactoria cuando se cumpla la propiedad de que C es abuelo de D.
abuelo(C,D) :- 
    (papa(C,A), papa(A,D)); (mama(C,A), papa(A,D)); (papa(C,A), mama(A,D));
    (mama(C,B), mama(B,D)), C \== D.

% | 11 | Regla nieto. 
nieto(E,F) :- hijo(E,Y), hijo(Y,F), E \== F.

% | 12 | Regla p.
%p([A|B]) :-
% ésto puede ser de ayuda =D print('→←')