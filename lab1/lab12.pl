node(a).
node(b).
node(c).
node(d).
node(e).
node(f).
node(g).
node(h).
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
app([],Ys,Ys).
app([X|Xs],Ys, [X|Zs]) :-
	app(Xs, Ys, Zs).
path(X,Y) :-
	edge(X,Y).
path(X,Y) :-
	edge(X,Z),
	path(Z,Y).
path(X,Y,Z) :-
	edge(X,Y).
path(X,Y,Z) :-
	edge(X,N),
	path(N,Y,W).

