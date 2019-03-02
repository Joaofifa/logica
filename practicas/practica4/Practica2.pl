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

%quevisitar(C,L) :- aqui_va_su_código_:)
%quevisitar2(C,L,M) :- aqui_va_su_código_:)

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

%padres(A,B,C) :- papa(A,C), mama(B,C). 

%hermano(M,N) :- aqui_va_su_código_:)

%mediohermano(D,E) :- aqui_va_su_código_:)

%hss(A,B) :- aqui_va_su_código_:)

%tio(S,T) :- aqui_va_su_código_:)

%primo(U,V) :- aqui_va_su_código_:)

%tutor(A,B) :- aqui_va_su_código_:)

%abuelo(C,D) :- aqui_va_su_código_:)

%nieto(E,F) :- aqui_va_su_código_:)

%p([A|B]) :- aqui_va_su_código_:)
% ésto puede ser de ayuda =D print('→←')