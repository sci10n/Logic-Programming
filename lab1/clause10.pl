%First we define all facts, all predicates has to be defined in order because prolog is super bad at parsing
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
%Like doesnt require any other non-fact predicates
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
like(nisse,X) :-
	woman(X),
	like(X,nisse).
%Happe requires like
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
