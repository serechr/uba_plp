% letraAusente(+P, -L)
letraAusente(P, L) :- letra(L), not(member(L, P)).

% reemplazar(+P, +S, +R, -PR)
reemplazar(P, S, R, PR) :- append(Prefijo, Resto, P),
    					   append(S, Sufijo, Resto),
    					   append(Prefijo, R, Aux),
    					   append(Aux, Sufijo, PR).

% rachaCompleta(+P, ?R)
rachaCompleta(P, R) :- append(Pre, Suf, P),
    				   append(R, T, Suf),
    				   todosIgualesA(R, X),
    				   not(last(Pre, X)),
    				   not(head(T, X)).

% todosIgualesA(+L, ?X)
todosIgualesA([X], X).
todosIgualesA([X|Xs], X) :- todosIgualesA(Xs, X).

% head(+L, ?H)
head([H|_], H).

% palabra(+M, -P)
palabra(M, P) :- M >= 1, desde(1, N), palabrasDeTamaño(N, M, P).

% desde(+X, -Y)
desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).

% palabrasDeTamaño(+N, +M, -P)
palabrasDeTamaño(N, M, [L|Ls]) :- N >= 1,
    							  letra(L),
    							  N1 is N-1,
    							  restoDePalabra(N1, M, 1, L, Ls).

% restoDePalabra(+N, +M, +ContRacha, +UltimaLetra, +Ls)
restoDePalabra(0,_,_,_,[]).
restoDePalabra(N, M, Cont, L, [L|Ls]) :- N > 0,
    									 N1 is N-1,
    									 Cont1 is Cont+1,
    									 Cont1 =< M,
    									 restoDePalabra(N1, M, Cont1, L, Ls).
restoDePalabra(N, M, Cont, Ultima, [L|Ls]) :- N > 0,
    										  letra(L),
    										  L \= Ultima,
    										  N1 is N-1,
    										  restoDePalabra(N1, M, 1, L, Ls).