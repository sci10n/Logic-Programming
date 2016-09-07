%Add factes about the edges of the graph
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
%Define predicate for appending lists where X is appended to Ys by inference of the append rule
app([],Ys,Ys).
app([X|Xs],Ys, [X|Zs]) :-
	app(Xs, Ys, Zs).
%Define predicate for path/2
%There is a path if there exists an edges between X,Y
path(X,Y) :-
	edge(X,Y).
%There is a path from X to Y if we can infere there is a path between Z,Y and X,Z.
path(X,Y) :-
	edge(X,Z),
	path(Z,Y).
%Define predicate for path/3
%Check if there is a edges between X,Y and append X to the path.
path(X,Y,Z) :-
	edge(X,Y),
	app([X],[],Z).
%Same as path/2 but appends X to the path
path(X,Y,Z) :-
	edge(X,N),
	path(N,Y,W),
	app([X],W,Z).
%Define predicate for pathn/3
%Extention of path/3 where the length of the path is returned
pathn(X,Y,Z) :-
	path(X,Y,W),
	length(W,Z).
