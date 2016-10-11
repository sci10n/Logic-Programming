:- set_prolog_flag(toplevel_print_options, [quoted(true),numbervars(true),portrayed(true),max_depth(20)]).
%goal(state([],[m,m,m,c,c,c])).
%start(state([m,m,m,c,c,c],[])).

start(state(3:3, 0:0)).
goal(state(0:0, 3:3)).

nonm( _, [] ).
nonm( E, [H|T] ) :-
	dif( E, H ), nonm( E, T ).


df_search(Path) :-
	start(S0),
	df_search([S0],Path).

df_search([S|Visited],[S|Visited]) :-
	goal(S).
df_search([S1|Visited], Path) :-
	action(S1,S2),
	nonm(S2,[S1|Visited]),
	df_search([S2,S1|Visited], Path).

bf_search(Path) :-
	start(S0),
	bf_search([[S0]],Path).
bf_search([[S|Path]|_], [S|Path]) :-
	goal(S).
bf_search([[S1|Path]|Partials], FinalPath) :-
	findall(S2,(action(S1,S2),nonm(S2,[S1|Path])), NewStates),
	expand([S1|Path], NewStates, NewPaths),
	append(Partials, NewPaths, NewPartials),
	bf_search(NewPartials,FinalPath).

expand(L1,L2,L3) :-
	findall([X|L1], member(X,L2), L3).

allowed_bank(M:C) :-
	M >= 0,
	C >= 0,
  	M >= C.
allowed_bank(0:C) :-
	C > 0.

allowed_state(L,R) :-
 	allowed_bank(L),
  	allowed_bank(R).


% action - move one M from left to right
action(state(LM1:LC1, RM1:RC1), state(LM2:LC1,RM2:RC1)) :-
	LM2 is LM1 - 1,
	RM2 is RM1 + 1,
	allowed_state(LM2:LC1, RM2:RC1).
% action - move one C from left to right
action(state(LM1:LC1, RM1:RC1), state(LM1:LC2,RM1:RC2)) :-
	LC2 is LC1 - 1,
	RC2 is RC1 + 1,
	allowed_state(LM1:LC2, RM1:RC2).
% action - move one M from right to left
action(state(LM1:LC1, RM1:RC1), state(LM2:LC1,RM2:RC1)) :-
	RM2 is RM1 - 1,
	LM2 is LM1 + 1,
	allowed_state(LM2:LC1,RM2:RC1).
% action - move one C from left to right
action(state(LM1:LC1, RM1:RC1), state(LM1:LC2,RM1:RC2)) :-
	RC2 is RC1 - 1,
	LC2 is LC1 + 1,
	allowed_state(LM1:LC2,RM1:RC2).

	% action - move one M from left to right
action(state(LM1:LC1, RM1:RC1), state(LM2:LC1,RM2:RC1)) :-
	LM2 is LM1 - 2,
	RM2 is RM1 + 2,
	allowed_state(LM2:LC1, RM2:RC1).
% action - move one C from left to right
action(state(LM1:LC1, RM1:RC1), state(LM1:LC2,RM1:RC2)) :-
	LC2 is LC1 - 2,
	RC2 is RC1 + 2,
	allowed_state(LM1:LC2, RM1:RC2).
% action - move one M from right to left
action(state(LM1:LC1, RM1:RC1), state(LM2:LC1,RM2:RC1)) :-
	RM2 is RM1 - 2,
	LM2 is LM1 + 2,
	allowed_state(LM2:LC1,RM2:RC1).
% action - move one C from left to right
action(state(LM1:LC1, RM1:RC1), state(LM1:LC2,RM1:RC2)) :-
	RC2 is RC1 - 2,
	LC2 is LC1 + 2,
	allowed_state(LM1:LC2,RM1:RC2).

% action - move one C from left to right
action(state(LM1:LC1, RM1:RC1), state(LM2:LC2,RM2:RC2)) :-
	RC2 is RC1 - 1,
	LC2 is LC1 + 1,
	RM2 is RM1 - 1,
	LM2 is LM1 + 1,
	allowed_state(LM2:LC2,RM2:RC2).
action(state(LM1:LC1, RM1:RC1), state(LM2:LC2,RM2:RC2)) :-
	RC2 is RC1 + 1,
	LC2 is LC1 - 1,
	RM2 is RM1 + 1,
	LM2 is LM1 - 1,
	allowed_state(LM2:LC2,RM2:RC2).
% action - move one C from right to left
%action(state(LM1:LC1, RM1:RC1), state(LM1:LC2,RM1:RC2)) :-
%	LC2 is LC1 + 1,
%	RC2 is RC1 - 1,
%	allowed_state(LM1:LC2, RM1:RC2).
%% action(state([],[]),state([],[])).
%% action(state(R,L),state(Rr,[Mr|L])) :-
%% 	member(Mr,R),
%% 	extract(Mr,R,Rr),
%% 	allowed_state(Rr,[Mr|L]).
%% action(state(R,L),state(Rr,[Mr1,Mr2|L])) :-
%% 	member(Mr1,R),
%% 	extract(Mr1,R,Rtr
%% 	member(Mr2,Rtr),
%% 	extract(Mr2,Rtr,Rr),
%% 	allowed_state(Rr,[Mr1,Mr2|L]).
%% action(state(R,L),state([Ml|R],Lr)) :-
%% 	member(Ml,L),
%% 	extract(Ml,L,Lr),
%% 	allowed_state([Ml|R],Lr).
%% action(state(R,L),state([Ml1,Ml2|R],Lr)) :-
%% 	member(Ml1,L),
%% 	extract(Ml1,L,Ltr),
%% 	member(Ml2,Ltr),
%% 	extract(Ml2,Ltr,Lr),
%% 	allowed_state([Ml1,Ml2|R],Lr).

%action(state([m,m,m,c,c,c],[]),state([m,m,c,c,c],[m,m]))
%action(state([m,m,m,c,c,c],[]),state([m,c],[m,c]))
%action(state([m,m,m,c,c,c],[]),state([m,m],[c,c]))

%action(state([m,c],[],state([m,m],[c]))
