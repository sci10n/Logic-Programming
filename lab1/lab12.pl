%Add facts about the graph
edge(a,b).
edge(a,c).
edge(b,c).
edge(c,d).
edge(c,e).
edge(d,f).
edge(d,h).
edge(e,f).
edge(e,g).
edge(f,g).
%Define predicate for list appending
app([],Ys,Ys).
app([X|Xs],Ys, [X|Zs]) :-
	app(Xs, Ys, Zs).
%Define predicate for path/2
path(X,Y) :-
	edge(X,Y).
path(X,Y) :-
	edge(X,Z),
	path(Z,Y).
%Define predicate for path/3
path(X,Y,Z) :-
	X == Y,
	app([X],[],Z).
path(X,Y,Z) :-
	edge(X,N),
	path(N,Y,W),
	app([X],W,Z).
%Define predicate for pathn/3
pathn(X,Y,Z) :-
	path(X,Y,W),
	length(W,Z).