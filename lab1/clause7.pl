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
happy(X) :-
	man(X),
	rich(X).
happy(X) :-
	woman(X),
	rich(X).