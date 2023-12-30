:- use_module(library(clpfd)).
% 1.(VV,EE) - ([v1,v2],[(v1,v2)])
% 2.G=[[]]  - [[v,v1,v2],[v2,v3,v4]]
% 3.факти   - връх(g1, a). ребро(g1, a, c). ориентирани

% връх(G,V) - V e връх в G
vertex((VV,_),V) :- elem(V,VV).
vertex(G,V) :- elem([V|_],G).

% ребро(G,V,W) - в графа G има ребро от V до W
edge((_,EE),V,W) :- elem((V,W),EE).
edge(G,V,W) :- elem([V|X],G), elem(W,X).

% върхове(G,VV) - VV e списък от върховете в G.
% Условие: G е напълно краен граф.
vertices_c(G,VV) :- findall(V, vertex(G,V), X), sort(X,VV). 

% път_от_до(G,X,V,W) - X e път в G, започващ от V и завършващ в W
% Условие: Известно е ограничение отгоре за дължината на Х или G e ацикличен краен граф.
path_from_to(G,[V],V,V) :- vertex(G,V).	% with one vertex
path_from_to(G,[V,V1|X],V,W) :- edge(G,V,V1), path_from_to(G,[V1|X],V1,W).

% път с дължина не повече от броя на върховете, вече работи и за циклични графи
random_path(G,X,V,W) :- M #=< N, vertices_c(G,VV), len(VV,N), len(X,M), path_from_to(G,X,V,W).

% разстояние(G,V,W,N) - най-къс път в G от V до W e списък с дължина N+1
% Условие: G e известен краен граф
dist(G,V,W,N) :- N #=< VN, vertices_c(G,VV), len(VV,VN), len(X,N+1), path_from_to(G,X,V,W), %намира някакъв път
    			 not( (K #< N, len(Y,K+1), path_from_to(G,Y,V,W)) ). % не съществува път по-къс от най-късия

diam(G,N) :- dist(G,_,_,N), % съществува някакъв път
    		 not( (K #> N, dist(G,_,_,K)) ). % не съществува по-дълъг от най-дългият път

% ОТ ИЗПИТ
part([],[],_,[]).
part([A|V],[B|Col],I,[A|X]) :- B #= I, part(V,Col,I,X).
part([_|V],[B|Col],I,X) :- B #\= I, part(V,Col,I,X).

generate_partition(G,A,S,B) :- vertices_c(G,VV), len(VV,N), len(Colors,N), Colors ins 0..2, label(Colors), %оцветяваме всеки връх
    						   part(VV,Colors,0,A),
							   part(VV,Colors,1,S),
    						   part(VV,Colors,2,B),
% Helpful funcs
elem(A,[A|_]).
elem(A,[_|X]) :- elem(A,X).

len([],0).
len([_|X],N) :- N #> 0, N #= N1 + 1, len(X,N1).

:- discontiguous vertex/2.
:- discontiguous edge/3.
vertex(g1, a). vertex(g1, b). vertex(g1, c). vertex(g1, d).
vertex(g1, e). vertex(g1, f). vertex(g1, g). vertex(g1, h).
edge(g1, a, b). edge(g1, a, c). edge(g1, b, d).
edge(g1, c, d). edge(g1, c, e). edge(g1, d, f).
edge(g1, e, f). edge(g1, e, g). edge(g1, f, h).
edge(g1, g, h).
vertex(g2, a). vertex(g2, b). vertex(g2, c). vertex(g2, d).
vertex(g2, e). vertex(g2, f). vertex(g2, g). vertex(g2, h).
edge(g2, a, a). edge(g2, a, e). edge(g2, a, b).
edge(g2, b, d). edge(g2, c, b). edge(g2, d, c).
edge(g2, d, e). edge(g2, e, f). edge(g2, f, d).
edge(g2, g, h). edge(g2, g, e). edge(g2, h, g).
edge(g2, h, f).
