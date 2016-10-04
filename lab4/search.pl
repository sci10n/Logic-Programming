delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail).
delete_one(Term, [Head|Tail], [Head|Result]) :-
  delete_one(Term, Tail, Result).


%% goal(S).
%% start(S).
%% action(S1,S2).

%% start(state([m,m,c,c])).
%% goal(state([],[m,m,c,c])).


%% allowed_bank(L) :-
%% 	I1 >= I2,
%% 	length(R1,I1),
%% 	length(R2,I2),
%% 	findall(X,misionaries(X,L),R1),
%% 	findall(Y,cannibals(Y,L),R2).

%% allowed_state(state(L,R)) :-
%% 	allowed_bank(L),
%% 	allowed_bank(R).

%% action_move_right(state(L,R),state(L2,[X|R])) :-
%% 	allowed_state(state(L,R)),
%% 	member(X,L),
%% 	delete_one(X,L,L2),
%% 	allowed_state(state(L2,[X|R])).


%action(state([m,m,m,c,c,c],[]),state([m,m,c,c,c],[m,m]))
%action(state([m,m,m,c,c,c],[]),state([m,c],[m,c]))
%action(state([m,m,m,c,c,c],[]),state([m,m],[c,c]))

%action(state([m,c],[],state([m,m],[c]))
