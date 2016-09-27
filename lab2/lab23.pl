%Define number and identifier literals
num(N).
id(I).

%Define evaluation of boolean expressions
boolean(S0,true).
boolean(S0,E1 > E2) :-
	expression(S0,E1,R1),
	expression(S0,E2,R2),
	R1 > R2.
boolean(S0,E1 == E2) :-
	expression(S0,E1,R1),
	expression(S0,E2,R2),
	R1 == R2.
boolean(S0,E1 < E2) :-
	expression(S0,E1,R1),
	expression(S0,E2,R2),
	R1 < R2.

%Execute given a program P and Binding Environment S0
execute(S0,P,Sn):-
	eval_expression(S0,P,R).
execute(S0,P,Sn):-
	eval_boolean(S0,P,R).

set(id(I),num(E)).

bind([], I, E, [set(I,E)]).
bind([set(I,A)|S0], I,E, [set(I,E)|S0]).
    
bind([set(H,A)|S0], I, E, [set(H,A)|Sn]) :-
    H \= I,
    bind(S0,I,E,Sn).

expression(S0,id(E),R) :-
	member(set(E,R), S0).
expression(S0,num(E),E).
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
command(S0,set(id(I),E),Sn) :-
	expression(S0,E,R),
	bind(S0,I,R,Sn).
command(S0,if(B,C1,C2),Sn) :-
    (boolean(S0,B),command(S0,C1,Sn)).
command(S0,if(B,C1,C2),Sn) :-
    ( \+ boolean(S0,B),command(S0,C2,Sn)).
command(S0,seq(C1,C2),Sn) :-
    command(S0,C1,Sr),
    command(Sr,C2,Sn).
command(S0,while(B,C),S0) :-
	not(boolean(S0,B)).
command(S0,while(B,C),Sn) :-
    boolean(S0,B),
    command(S0,C,Sr),
    command(Sr,while(B,C),Sn).

%Define function for intersection, union, and powerset
not_exists(E,[]).
not_exists(E,[H|T]) :-
	dif(E,H),
	not_exists(E,T).

set([]).
set([H|T]) :-
	not_exists(H,T),
	sort([H|T],[H|T]),
	set(T).

%Intersection
intersect([],[],[]).
intersect(_,[], []).
intersect([],_, []).
intersect([A|Ta],[B|Tb],[A|Tc]) :-
	set([A|Ta]),
	set([B|Tb]),
	A == B,
	intersect(Ta,Tb,Tc).
intersect([A|Ta],[B|Tb],T) :-
	set([A|Ta]),
	set([B|Tb]),
	A < B,
	intersect(Ta,[B|Tb],T).
intersect([A|Ta],[B|Tb],T) :-
	set([A|Ta]),
	set([B|Tb]),
	B < A,
	intersect([A|Ta],Tb,T).

union([],[],[]).
union(A,B,C) :-
	set(A),
	set(B),
	append(A,B,AB),
	setof(X,member(X,AB),C).

powerset([],[[]]).
powerset(A,C) :-
	set(A),
	setof(X,(member(X,A), subseq0(A,X)),C).