
grandparent(X,Y) :- parent(X,Z) , parent(Z,Y).

parent(liz, billy).
parent(liz, charley).
parent(charley, will).
parent(phil, charley).
parent(charley,ed).

?- grandparent(liz,ed).





female(liz).
female(di).
male(X) :- not(female(X)).

parent(di, will).

grandmother(X,Y) :- grandparent(X,Y), female(X).

married(X,Y) :- married(Y,X).


append([],X,X).
append([X|L],Y,[X|L1]) :- append(L,Y,L1).

?-append([1,2,3],[4,5,6],A).

?-append([1 | L],B,[1,2,3,4]).

member(X,[X|L]).
member(X,[Y|L]) :- member(X,L).
