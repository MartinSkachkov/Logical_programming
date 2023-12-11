% дължина(L, N) - N е дължината на списъка L
% Условие: Известно е ограничение отгоре за дължината на L
%          или за N
дължина([], 0).
дължина([_|T], N) :- N #>= 0, N #= N1 + 1, дължина(T,N1).

% nth(L, N, A) - A е N-тият елемент на списъка L
% Условие: Известно е ограничение отгоре за дължината на L
%          или за N
nth([A|_], N, A) :- N #= 1.
nth([_|T], N, A) :- N #> 0, N1 #= N - 1, nth(T, N1, A).

% сума(L, S) - S е сума на елементите на списъка L
% Условие:  известно е ограничение отгоре за дължината на L
сума([], 0).
сума([H|T], S) :- S #= M + H, сума(T, M).

% A in 5..13 <=> A #>= 5, A #=< 13
% [A,B,C] ins 5..13 <=> A in 5..13, B in 5..13, C in 5..13.
% от_до(X, A, B) - X е списък, чиито елементи са числа между A и B
% Условие:  известно е ограничение отгоре за дължината на L
от_до([],_,_).
от_до([H|T], A, B) :- H #>= A, H #=< B, от_до(T,A,B).

% ограничен_списък(N, L) - L е списък с дължина не повече от N,
%                          чиито елементи са естествени числа, не
%                          по-големи от N
%
% Условие: Известно е N
ограничен_списък(N, L) :- K #=< N, дължина(L, K), L ins 0..N.

% факториел(N, F) - F = N!
% Условие: известно е N
факториел(0, 1).
факториел(N, F) :- N #> 0, F #= N * F1, N #= 1 + N1, факториел(N1,F1).

% compress (L, S):- the list S is obtained from the
%                   list L by compressing repeated
%                   occurrences of elements into a single copy
%                   of the element.
% Условие: известно е ограничение отгоре за дължината на L
compress([], []).   % Base Case: L = []
compress([X], [X]). % Base Case: L has only one element
compress([X,X|Xs], S) :- compress([X|Xs], S).
compress([X,Y|Ys], [X|S]) :- X \= Y, compress([Y|Ys],S).

% range(X, Y, L) :- creates a list L containing all the integers within
%                   the range [X,Y]
range(X,X,[X]). % Base Case: When the range is [3,3] for expl
range(X,Y,[X|L]) :- X #< Y, X1 #= X + 1, range(X1, Y, L).

% split (L, N,F,S) :- the list F contains the first N elements
%                     of the list L, the list S contains the remaining elements.
split(L,0,[],L). % Base Case: N = 0
split([X|Tail], N, [X|Ys], S) :- N #> 0, N1 #= N - 1, split(Tail, N1, Ys, S).

% rotate (L,N,X) :- the list X is
%                   obtained from the list L by
%                   rotating the elements of L, N places to the left.
% N >= 0, Rotate to the left
rotate(L,N,X) :- N #>= 0, length(L,S), P #= N mod S, rotate_left(L,P,X).

% N < 0, Abstractly rotate to the right
rotate(L,N,X) :- N #< 0, length(L,S), P #= (S + (N mod S)) mod S, rotate_left(L,P,X).

rotate_left(L,0,L).
rotate_left(L,P,X) :- P #> 0, split(L,P,S1,S2), append(S2,S1,X).