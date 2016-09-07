insert(X,[],[X]).
insert(X,[A|Y],[X|[A|Y]]) :-
	X =< A.
insert(X,[A|Y],[A|W]) :-
	X > A,
	insert(X,Y,W).

isort([],[]).
isort([A|X],Y) :-
	isort(X,Z),
	insert(A,Z,Y).

pivot(P,[],L,G,W) :-
    append(L,[P|G],W).
pivot(P,L,W) :-
    pivot(P,L,[],[],W).
pivot(P,[X|Y],L,G,W) :-
    X =< P,
    pivot(P,Y,[X|L],G,W).
pivot(P,[X|Y],L,G,W) :-
    X > P,
    pivot(P,Y,L,[X|G],W).    
