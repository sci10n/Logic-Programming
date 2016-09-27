% Facts about the graph, which
% is basically a set of edges.
% Def: An edge between a -- b.
%
edge(a,b). edge(a,c).
edge(b,c).
edge(c,d). edge(c,e).
edge(d,f). edge(d,h).
edge(e,f). edge(e,g).
edge(f,g).

% Appends a list X onto a
% list Y, put into Z list.
% Def: append lists X & Y.
%
app([],Ys,Ys).
app([X|Xs],Ys, [X|Zs]) :-
    app(Xs, Ys, Zs).

% A path between vertices X
% and Y exists if edges are
% directly between X and Y.
% Def: path if edge X -- Y.
%
path(X,Y) :-
    edge(X,Y).

% Also a path, if intermediary
% path exists between vertices
% X and Y in the form of edges
% connected by some Z vertices
% Def: path if edge X--Z and a
% path already exists between,
% Z--Y (recursive definition).
%
path(X,Y) :-
    edge(X,Z),
    path(Z,Y).

% Same as the path/2 above, but
% also store the path it takes,
% appending this into list Z...
%
path(X,Y,Z) :-
    edge(X,Y),
    app([X],[],Z).
% Continued...
path(X,Y,Z) :-
    edge(X,N),
    path(N,Y,W),
    app([X],W,Z).

% Path length.
pathl(X,Y,Z) :-
    path(X,Y,W),
    length(W,Z).

% -------------------------
% ------EXAMPLE QUERY------
% -------------------------
%
% Is there a path between 'a' and 'h'?
% ------------------------------------
%
% ?- path(a, h).
% yes
%
% What paths does 'e' lead towards to?
% ------------------------------------
%
% ?- path(e, X).
% X = f ? ;
% X = g ? ;
% X = g ? ;
% no
%
% What are the paths between 'a' and 'h'?
% ---------------------------------------
%
% ?- path(a, h, S).
% S = [a,b,c,d] ? ;
% S = [a,c,d] ? ;
% no
%
% How far away are the paths between 'a' and 'h'?
% -----------------------------------------------
%
% ?- pathl(a, h, L).
% L = 4 ? ;
% L = 3 ? ;
% no
