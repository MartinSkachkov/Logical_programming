% четно_позиционирани(L1, L2) - L2 е списък от елементите на четна позиция в L1
% Условие: известно е ограничение отгоре за дължината на L1 или
%          дължината на L2
четно_позиционирани([], []).
четно_позиционирани([_|Tail1], L2):-нечетно_позиционирани(Tail1,L2).

% нечетно_позиционирани(L1, L2) - L2 е списък от елементите на нечетна позиция в L1
% Условие: известно е ограничение отгоре за дължината на L1 или
%          дължината на L2
нечетно_позиционирани([], []).
нечетно_позиционирани([X|Tail1], [X|Tail2]):-четно_позиционирани(Tail1,Tail2).

% p(X,Y) :- Y се получава като разменим в X елементите на четна позиция с
%           елементите на нечетна позиция.
p(X,Y):-четно_позиционирани(X,V),
        нечетно_позиционирани(X,W),
        четно_позиционирани(Y,W),
        нечетно_позиционирани(Y,V).


% Ако прочетем горното като математическо твърдение, забелязваме, че то е
% вярно.  Това ни дава частичната коректност на предиката p.

% За да получим пълна коректност, трябва да видим, че са изпълнени условията
% в спецификацията на предикатите, които използваме.  В нашия случай това са
% предикатите четно_позиционирани и нечетно_позиционирани.  Условията в
% тяхната спецификация казват, че поне един от аргументите им трябва да бъде
% с известна дължина.  Да видим кога ще бъдат изпълнени тези условия.

% (1): X или V трябва да бъдат списък с известна дължина.  Но V е
% променлива, която на това място се среща за пръв път, така че няма как да
% бъде с известна дължина.  Значи X трябва да бъде с известна дължина.  След
% изпълнението на (1) променливата V също ще стане с известна дължина.

% (2): Вече знаем, че X трябва да бъде с известна дължина.  Така че в този
% ред условието е изпълнено.  След изпълнението на (2) променливата W също
% става списък с известна дължина.

% (3): Условието е изпълнено, защото W вече е списък с известна дължина.
% След изпълнението на (3) променливата Y също става списък с известна
% дължина (ако дължината на W е n, то дължината на Y е 2n или 2n+1.

% (4): Условието е изпълнено, защото когато дойдем до този ред Y и V са
% списъци с известна дължина.

% Горните разсъждения показват, че за да работи p(X,Y), е достатъчно X да
% бъде списък с известна дължина.  Това е условието за коректност на
% предиката p.

% При следващия въпрос сме спазили това условие за p.  Затова компютърът
% намира верният отговор и правилно отговаря, че втори верен отговор няма:

%% ?- p([1,2,3,4],X).
%% X = [2, 1, 4, 3] ;
%% false.

% Обаче при следващото извикване не сме спазили условието за коректност на p.
% Дори в този случай компютърът няма да ни излъже (защото имаме частична
% коректност), само че след като намери верният отговор, компютърът ще
% продължи да търси други отговори без да може да се сети, че такива няма:

%% ?- p(X,[1,2,3,4]).
%% X = [2, 1, 4, 3] ;
%%   Action (h for help) ? a
%% abort
%% % Execution Aborted


%дали елемент X е елемент на списък
% Условие: Известно е ограничение отгоре за дължината на списъка
елем(X,[X|Tail]).
елем(X,[Head|Tail]):-елем(X,Tail).

%сливане на списъци
% Условие: Известно е ограничение отгоре за дължината на L1 или дължината
%          на L3
слей([],L2,L2).
слей([X|L1],L2,[X|L3]):-слей(L1,L2,L3).

%добавяне на елемент към списък в началото
добави(X,L,[X|L]).

%премахване на елемент от списък
% Условие: Известно е ограничение отгоре за дължината на L2 или дължината
%          на L3
изтрий(X,[X|Tail],Tail).
изтрий(X,[Y|Tail],[Y|Tail1]):-изтрий(X,Tail,Tail1).

%предпоследен елемент на списък
% Условие: Известно е ограничение отгоре за дължината на списъка
предпоследен(X,[X|[Y]]).
предпоследен(X,[Y|Tail]):-предпоследен(X,Tail).
%друг начин
предпоследен2(X,L):-слей(_,[X,_],L).

%списъкът L1 е префикс на L2
% Условие: известно е ограничение отгоре за дължината на X или на Y
префикс([],L2).
префикс([X|Tail1],[X|Tail2]):-префикс(Tail1,Tail2).
%друг начин
префикс2(L1,L2):-слей(L1,_,L2).

%Първи начин
%пермутация(L1,L2) - списъкът L1 е пермутация на списъка L2
% Условие: известно е ограничение отгоре за дължината на L1
пермутация([],[]).
пермутация([X|Tail1],L2):-пермутация(Tail1,List), вмък(X,List,L2).

%вмък(X, L1, L2) - L2 може да се получи като вмъкнем X на произволна позиция в L1
% Условие: известно е ограничение отгоре за дължината на L1 или дължината на L2
вмък(X,L1,[X|L1]).
вмък(X,[Y|Tail1],[Y|Tail2]):-вмък(X,Tail1,Tail2).

%Втори начин
%пермутация2(L1,L2) - списъкът L1 е пермутация на списъка L2
% Условие: известно е ограничение отгоре за дължината на L2
пермутация2([],[]).
пермутация2(L1,[X|Tail2]):-пермутация2(List,Tail), измък(X,L1,List).

% За измък може пак да използваме структурна индукция
измък(X, [X|Tail1], Tail1).
измък(X, [Y|Tail1], [Y|Tail2]) :- измък(X, Tail1, Tail2).
%друг начин
измък2(X,L1,L2):-вмък(X,L2,L1).

% Условието при предикатите пермутация и пермутация2 беше да е известна
% дължината на първия аргумент.  От математическа гледна точка обаче ако X е
% пермутация на Y, то Y е пермутация на X.  Т.е. не би трябвало да има
% значение дали е известен първия или втория аргумент.  Да направим
% предикат, който може да се използва и в двете посоки.

% равна_дължина(L1, L2) - списъците L1 и L2 са с равна дължина.
равна_дължина([], []).
равна_дължина([_|Tail1], [_|Tail2]):-равна_дължина(Tail1,Tail2).

двупосочна_пермутация(L1, L2):-равна_дължина(L1,L2), пермутация(L1,L2).

%% enable highlighting of ambig chars.