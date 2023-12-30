:- use_module(library(clpfd)).
% безкраен_генератор(X) :-  nat(N), краен_генератор(N,X).
nat(N) :- N #= 0; nat(N-1).

% pairs([A,B]) - генерира всички двуелементни списъци от естествени числа
pairs([A,B]) :- nat(N), 0 #=< A, A #=< N, 0 #=< B, B #=< N, label([A,B]).

% lists(X) - генерира всички списъци от естествени числа
lists(X) :- nat(N), K #=< N, len(X,K), X ins 0..N, label(X).  
    
% целите числа
int(X) :- nat(N), (X #= N; X #= -N), label([X]).
    
% Helpful funcs
len([],0).
len([_|X],N) :- N #> 0, N #= N1 + 1, len(X,N1).
