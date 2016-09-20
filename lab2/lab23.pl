
num(N) :-
	number(N).
id(I) :-
	string(I).


%%eval_expression(S0,E,R) :-
%%eval_expression(S0,E1,R1),
%%eval_expression(S0,E2,R2),
%%R is R1 + R2.

eval_boolean(S0,true).
%eval_boolean(false).
eval_boolean(S0,E1 > E2) :-
	expression(S0,E1,R1),
	expression(S0,E2,R2),
	E1 > E2.
eval_boolean(S0,E1 == E2) :-
	expression(S0,E1,R1),
	expression(S0,E2,R2),
	E1 == E2.
eval_boolean(S0,E1 < E2) :-
	expression(S0,E1,R1),
	expression(S0,E2,R2),
	E1 < E2.
%%	eval_expression(S0,E2, R2),
%%	R1 > R2.

execute(S0,P,Sn):-
	eval_expression(S0,P,R).
execute(S0,P,Sn):-
	eval_boolean(S0,P,R).

set(I,E) :-
	id(I),
	num(E).

bind([], I, E, [set(I,E)]).
bind([H|S0], I, E, [H|Sn]) :-
	bind(S0,I,E,Sn).
	
%[set("x",1), set("y",2)] 

expression(S0,E,R) :-
	id(E),
	member(set(E,R), S0).
expression(S0,E,E) :-
	num(E).
expression(S0, E1 + E2, R) :-
	expression(S0, E1, R1),
	expression(S0, E2, R2),
	R is (R1 + R2).
expression(S0, E1 - E2, R) :-
	expression(S0, E1, R1),
	expression(S0, E2, R2),
	R is (R1 - R2).
expression(S0, E1 * E2, R) :-
	expression(S0, E1, R1),
	expression(S0, E2, R2),
	R is (R1 * R2).
expression(S0, - E, R) :-
	expression(S0, E, R1),
	R is ( R1 * -1).