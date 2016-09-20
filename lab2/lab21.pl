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

pivot(_,[],L,G,L,G).

pivot(P,[X|Y],L,G,Lr,Gr) :-
    X =<P,
    pivot(P,Y,[X|L],G, Lr, Gr).
pivot(P,[X|Y],L,G, Lr,Gr) :-
    X > P,
    pivot(P,Y,L,[X|G],Lr,Gr).    

qsort([],[]).
qsort([P|Li],Lo) :-
	pivot(P,Li,[],[],Lr,Gr),
	qsort(Lr,QLr),
	qsort(Gr,QGr),
	append(QLr,[P|QGr],Lo).