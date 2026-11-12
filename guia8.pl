%% Ejercicio 4

% juntar(?Lista1,?Lista2,?Lista3)
juntar([], L, L).
juntar([X|L1], L2, [X|L3]) :- juntar(L1, L2, L3).

%% Ejercicio 5

% last(?L, ?U)
last(L, X) :- append(_, [X], L).

% first(?L, ?P)
first([H|_], H).

% reverse(+L, ?R)
reverse([], []).
reverse([H|T], L) :- reverse(T, L1), append(L1, [H], L).

prefijo(P,L) :- append(P,_,L).

sufijo(S,L) :- append(_,S,L).

% sublista(?S, +L)
sublista(Q,L) :- prefijo(P,L), sufijo(S,L), append(P,Q,L1), append(L1,S,L).

% pertenece(?X, +L)
%pertenece(X,[X|_]).
%pertenece(X,[_|T]) :- pertenece(X,T).

% pertenece(?X, +L)
pertenece(X,L) :- append(_,[X|_],L).

%% Ejercicio 6

% aplanar(+Xs, -Ys)
%aplanar([X],[X]).
%aplanar([H|_], L) :- member(H1,L), aplanar(H,H1).

%intersección(+L1, +L2, -L3)
interseccion([H|_],[H|_],L3) :- member(H,L3).
interseccion([H|_],[_|T],L3) :- member(H,T), member(H,L3).
interseccion([_|T1],[_|T2],L3) :- interseccion(T1,T2,L3).


%! intersection(+C1, +C2, -C3)
interseccion([], _, []).
interseccion([X|T], C, [X|R]) :- member(X, C),     interseccion(T, C, R).
interseccion([X|T], C, R)     :- not(member(X, C)), interseccion(T, C, R).

%interseccion([],_,[]).
%interseccion([H|T],L2,[H|L3]) :- member(H,L2), interseccion(T,L2,L3).
%interseccion([H|T],L2,L3) :- not(member(H,L2)), not(member(H,L3)), interseccion(T,L2,L3).

% partir(N, L, L1, L2)

partir(0,L,[],L).
partir(N,L,L1,L2) :- N > 0, N1 is N - 1, partir(N1,L,L3,[H|L2]), append(L3,[H],L1).

% borrar(+ListaOriginal, +X, -ListaSinXs)
borrar([],_,[]).
borrar([X|T],X,L) :- borrar(T,X,L).
borrar([Y|T],X,[Y|L]) :- X \= Y, borrar(T,X,L).

% sacarDuplicados(+L1, -L2)
sacarDuplicados1([],[]).
sacarDuplicados1([X|T],T1) :- member(X,T), sacarDuplicados1(T,T1).
sacarDuplicados1([X|T],[X|T1]) :- not(member(X,T)), sacarDuplicados1(T,T1).

% sacarDuplicados(+L1, -L2)
sacarDuplicados([],[]).
sacarDuplicados([X|T],L) :- sacarDuplicados(T,Ts), borrar(Ts,X,Ls), append([X],Ls,L).

borrar1([],_,[]).
borrar1([X|T],X,T).
borrar1([Y|T],X,[Y|L]) :- X \= Y, borrar(T,X,L).

% permutación(+L1, ?L2)
permutacion([X],[X]).
permutacion([H|T],L) :- member(H,L), permutacion(T,L1), borrar1(L,H,L1).

%% INSTANCIACIÓN Y REVERSIBILIDAD
% desde(+X,-Y)
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

% desdeReversible(+X, ?Y)
desdeReversible(X,Y) :- var(Y), desde(X,Y).
desdeReversible(X,Y) :- nonvar(Y), X =< Y.

% intercalar(?L1, ?L2, ?L3)
intercalar([],[],[]).
intercalar([H1|T1],[H2|T2],L3) :- append([H1,H2],CR,L3), intercalar(T1,T2,CR).

%% árbol binario
% vacio(?AB).
vacio(nil).

% raiz(?AB, ?R)
raiz(bin(_,V,_),V).

% altura(?AB, ?N)
altura(nil, 1).
altura(bin(Izq, V, Der), N) :- N is N1+1, 

%% GENERATE AND TEST