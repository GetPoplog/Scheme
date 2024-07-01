

member(X,[X|L]).
member(X,[Y|L]) :- member(X,L).

qval(Y) :- member(Y,[1,2,3,4]).

queens([Y1,Y2,Y3,Y4]) :-  qval(Y1),qval(Y2),qval(Y3),qval(Y4),
          ok([Y1,Y2,Y3,Y4]).

ok(L) :- ok1(L), ok_diag(1,L).

ok1([X|L]) :- not(member(X,L)),ok1(L).
ok1([]).

........ so how do we write ok_diag?
........ remember X is Y+Z
