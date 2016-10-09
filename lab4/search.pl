%% goal(S).
%% start(S).
%% action(S1,S2).

%% start(state([m,m,c,c])).
%% goal(state([],[m,m,c,c])).
%% contains([],[]).
%% contains(X,[X|T]) :-
%% 	contains(X,T).
%% contains(X,[H|T]) :-
%% 	dif(X,H),
%% 	contains(X,T).

extract(_,[],[]).
extract(E,[H|T],[H|Rt]):-
	dif(E,H),
	extract(E,T,Rt).
extract(E,[E|T],T).

allowed_bank(L) :-
  	findall(X,(member(X,L), X = m),R1),
 	findall(Y,(member(Y,L), Y = c),R2),
  	length(R1,I1),
  	length(R2,I2),
  	I1 >= I2.
allowed_bank(L) :-
  	findall(X,(member(X,L), X = m),[]),
	findall(Y,(member(Y,L), Y = c),R),
	length(R,I),
	I >= 1.

allowed_state(L,R) :-
 	allowed_bank(L),
  	allowed_bank(R).

action([],[],[],[]).
action(R,L,Rr,[Mr|L]) :-
	member(Mr,R),
	extract(Mr,R,Rr),
	allowed_state(Rr,[Mr|L]).
action(R,L,Rr,[Mr1,Mr2|L]) :-
	member(Mr1,R),
	extract(Mr1,R,Rtr),
	member(Mr2,Rtr),
	extract(Mr2,Rtr,Rr),
	allowed_state(Rr,[Mr1,Mr2|L]).
action(R,L,[Ml|R],Lr) :-
	member(Ml,L),
	extract(Ml,L,Lr),
	allowed_state([Ml|R],Lr).
action(R,L,[Ml1,Ml2|R],Lr) :-
	member(Ml1,L),
	extract(Ml1,L,Ltr),
	member(Ml2,Ltr),
	extract(Ml2,Ltr,Lr),
	allowed_state([Ml1,Ml2|R],Lr).

%action(state([m,m,m,c,c,c],[]),state([m,m,c,c,c],[m,m]))
%action(state([m,m,m,c,c,c],[]),state([m,c],[m,c]))
%action(state([m,m,m,c,c,c],[]),state([m,m],[c,c]))

%action(state([m,c],[],state([m,m],[c]))
