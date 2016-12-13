bitor(1, [1|_]).
bitor(0, [0]).
bitor(B, [0|T]) :- bitor(B, T).

bitand(0, [0|_]).
bitand(1, [1]).
bitand(B, [1|T]) :- bitand(B, T).

bitcomposition(0, []).
bitcomposition(N, [H|T]) :-
    H is N mod 2,
    Shift is N // 2,
    bitcomposition(Shift, T).


inc(N, NPlusOne) :- add(NPlusOne, N, 1).

add(Z, X, Y) :- var(X), X is Z - Y.
add(Z, X, Y) :- var(Y), Y is Z - X.
add(Z, X, Y) :- Z is X + Y.
