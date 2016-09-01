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