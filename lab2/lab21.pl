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