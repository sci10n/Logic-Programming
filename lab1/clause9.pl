beautiful(nisse).
beautiful(peter).
beautiful(ulrika).
kind(bosse).
rich(bettan).
rich(nisse).
strong(bettan).
strong(bosse).
strong(peter).
man(bosse).
man(peter).
man(nisse).
woman(ulrika).
woman(bettan).
like(X,Y) :-
	man(X),
	woman(Y),
	beautiful(Y).
like(urlika,X) :-
	man(X),
	rich(X),
	kind(X).
like(ulrika,X) :-
	man(X),
	beautiful(X),
	strong(X).
happy(X) :-
	man(X),
	like(X,Y),
	woman(Y),
	like(Y,X).
happy(X) :-
	woman(X),
	like(X,Y),
	man(Y),
	like(Y,X).
