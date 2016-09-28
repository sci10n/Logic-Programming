%Inorder-insert element in list
insert(X,[],[X]).
insert(X,[A|Y],[X|[A|Y]]) :-
	X =< A.
insert(X,[A|Y],[A|W]) :-
	X > A,
	insert(X,Y,W).

%Insertion sort. 
isort([],[]).
isort([A|X],Y) :-
	isort(X,Z),
	insert(A,Z,Y).

%Pivot clauses used by the quick-sort algorithm
pivot(_,[],L,G,L,G).

pivot(P,[X|Y],L,G,Lr,Gr) :-
    X =<P,
    pivot(P,Y,[X|L],G, Lr, Gr).
pivot(P,[X|Y],L,G, Lr,Gr) :-
    X > P,
    pivot(P,Y,L,[X|G],Lr,Gr).
        
%quick-sort clauses. 
qsort([],[]).
qsort([P|Li],Lo) :-
	pivot(P,Li,[],[],Lr,Gr),
	qsort(Lr,QLr),
	qsort(Gr,QGr),
	append(QLr,[P|QGr],Lo).

% -------------------------
% ------EXAMPLE QUERY------
% -------------------------
%
% Sort the list [2,1,4,3,5] using isort
% -----------------------------------------
%
% ?- isort([2,1,4,3,5],X).
% X = [1,2,3,4,5]
%
% Sort the list [2,1,4,3,5] using qsort
% ------------------------------------
%
% ?- qsort([2,1,4,3,5],X).
% X = [1,2,3,4,5] 
%
% Check if [1, 3, 5, 9] is a valid sort of the list [3, 1, 9, 5] using isort
% -------------------------------------------------------------------------
%
% ?- isort([3, 1, 9, 5],[1, 3, 5, 9]).
% yes;
%
% Check if [1, 3, 5, 9] is a valid sort of the list [3, 1, 9, 5] using qsort
% -------------------------------------------------------------------------
%
% ?- qsort([3, 1, 9, 5],[1, 3, 5, 9]).
% yes;