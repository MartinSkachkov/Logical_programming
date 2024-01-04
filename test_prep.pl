:- use_module(library(clpfd)).
len([],0).
len([_|X],N) :- N #> 0, N #= N1 + 1, len(X,N1).

compare_lists(X, Y) :- 
    forall(elem(A, X), elem(A, Y)),
    forall(elem(B, Y), elem(B, X)).

elem(A,[A|_]).
elem(A,[_|X]) :- elem(A,X).

div(N,A) :- N #\= 0, N * _K #= A, label([N]).

gcd(N,A,B) :- div(N,A), div(N,B), forall((div(M,A),div(M,B)), M #=< N).

% X е подсписък на Y
sublist([],_).
sublist([A|X],[A|Y]) :- sublist(X,Y).
sublist(X,[_|Y]) :- sublist(X,Y).

% p(L) - no даден списък L от списьци проверява дали за
% всеки два елемента на L съществува трети елемент,
% съдържащ общите елементи на другите два.
% за всеки два подсписъка X и Y на L, съществува Z от L, такъв че за всяко E, което е елемент на X и Y е елемент и на Z 
p1(L) :- forall((sublist([X,Y],L)), (member(Z,L), forall((member(E,X),member(E,Y)),member(E,Z)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

div_list(N,X) :- once(member(B,X)), N * _M #= B, label([N]), forall(member(A,X), N * _K #= A).

gcd_list(N,X) :- div_list(N,X), forall(div_list(M,X), M #=< N).

% p(L, N) - по списък от естествени числа L и
% естествено число N проверява дали има N елемента
% al..aN на L, чийто НОД ce различава от НОД на кои да
% e N-1 елемента на L.

p2(L,N) :- len(AA,N), sublist(AA,L), gcd_list(Gcd_AA,AA),
    	  forall((len(BB,N-1),sublist(BB,L)), (Gcd_BB #\= Gcd_AA, gcd_list(Gcd_BB,BB))).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
% A ce погльща от В (списъци от числа), ако сборьт на
% всеки два елемента на A ce съдьржа в В.

поглъща_се_от(X,Y) :- forall((sublist([A,B],X), label([A,B])), (N #= A + B, member(N,Y))).
    
всички_се_поглъщат_от_следващи(X) :- forall(append(_,[A|Y],X), forall(member(B,Y),поглъща_се_от(A,B))).

подиножество_разлика(_,_,[]).
подиножество_разлика(X,Y,[A|Z]) :- member(A,X), not(member(A,Y)), подиножество_разлика(X,[A|Y],Z).    
    
подмножество(X, Y) :- подиножество_разлика(Y, [], X).
% q(L,S) - no даден списък L от списъци от числа
% генерира в S максимална (по дължина) редица от
% различни елементи на L, в която всеки елемент ce
% погльща от всички следващи елементи на S.
q(L, 5) :-
     подмножество(S, L), всички_се_поглъщат_от_следващи(S),
    forall((подиножество(S1, L),всички_се_поглъщат_от_следващи(S1)),
            (len(S, LenS), len(S1, LenS1),
             LenS1 #=< LenS)).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
a(N,A) :-
    N #=0, A #= 1;
    N #> 0, A #= 2*A1+N, a(N-1,A1).
 b(N,B) :-
    N #= 0, B #= 1;
    N #> 0, B #= 3*B1+N*N-1, b(N-1,B1).

nat(N) :- N #= 0; nat(N-1).

% p(N) - генерира числата от вида ai+bj. 
p4(N) :- nat(K), I in 0..K, J in 0..K, N #= AI + BJ, a(I,AI), b(J,BJ), label([N]).     
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% monot_pref(X) - X e списък от списъци,
% 				  всеки от които e префикс на следващия

monot_pref([]).
monot_pref([_]).
monot_pref([A,B|X]) :- append(A,_,B), monot_pref([B|X]).

% p(L) - генерира всички списъци от списъци от
% естествени числа между 0 и 99, такива че всеки
% елемент e префикс на следващия.

p5(L) :- nat(K), къс_от_къси(K,K,L), monot_pref(L).%, append(_,[Последен],L), label(Последен).

къс_от_къси(N,M,L) :- LenL #=< N, len(L,LenL), (L=[] ; L=[A|X], LenA #=< M, len(A,LenA), A ins 0..2, къс_от_къси(N-1,M,X)).












    
    

