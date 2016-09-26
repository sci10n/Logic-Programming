%Define number and identifier literals
num(N) :-
	number(N).
id(I).

%Define evaluation of boolean expressions
boolean(S0,true).
boolean(S0,E1 > E2) :-
	expression(S0,E1,R1),
	expression(S0,E2,R2),
	E1 > E2.
boolean(S0,E1 == E2) :-
	expression(S0,E1,R1),
	expression(S0,E2,R2),
	E1 == E2.
boolean(S0,E1 < E2) :-
	expression(S0,E1,R1),
	expression(S0,E2,R2),
	E1 < E2.

%Execute given a program P and Binding Environment S0
execute(S0,P,Sn):-
	eval_expression(S0,P,R).
execute(S0,P,Sn):-
	eval_boolean(S0,P,R).

set(I,E) :-
	id(I),
	num(E).

bind([], I, E, [set(I,E)]).
bind([set(I,A)|S0], I,E, [set(I,E)|S0]).
    
bind([set(H,A)|S0], I, E, [set(H,A)|Sn]) :-
    H \= I,
    bind(S0,I,E,Sn).

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

%Define commands
command(S0,skip,S0).
command(S0,set(I,E),Sn) :-
	id(I),
	expression(S0,E,R),
	bind(S0,I,R,Sn).
command(S0,if(B,C1,C2),Sn) :-
    (boolean(S0,B),command(S0,C1,Sn)).
command(S0,if(B,C1,C2),Sn) :-
    ( \+ boolean(S0,B),command(S0,C2,Sn)).
command(S0,seq(C1,C2),Sn) :-
    command(S0,C1,Sr),
    command(Sr,C2,Sn).
command(S0,while(B,C),Sn) :-
    boolean(S0,B),
    command(S0,C,Sr),
    command(Sr,while(B,C),Sn).
