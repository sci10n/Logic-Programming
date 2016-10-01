middle(X, [X]).
middle(X, [H|T]) :-
    append(M, [L], T),
    middle(X, M).

% -------------------------
% ------EXAMPLE QUERY------
% -------------------------
% --------VERSION 1--------
%
% ----Program----
% middle(X, [X]).
% middle(X, [H|T]) :-
%     append(M, [L], T),
%     middle(X, M).
%
% ?- middle(X, [a, b, c]).
% X = b ? ;
% no
%
% ?- middle(a, X).
% X = [a] ? ;
% X = [_A,a,_B] ? ;
% X = [_A,_B,a,_C,_D] ?
% (infinite results...)
% yes
%
% --------VERSION 2--------
%
% ------Program------
% middle(X, [H|T]) :-
%     append(M, [L], T),
%     middle(X, M).
% middle(X, [X]).
%
% ?- middle(X, [a, b, c]).
% X = b ? ;
% no
%
% ?- middle(a, X).
% X = [_A,a,_B] ? ;
% X = [_A,_B,a,_C,_D] ?
% (infinite results...)
% yes
%
% --------VERSION 3--------
%
% ------Program------
% middle(X, [H|T]) :-
%     middle(X, M),
%     append(M, [L], T).
% middle(X, [X]).
%
% ?- middle(X, [a, b, c]).
% Resource error: insufficient memory
% (non-terminating query it seems???)
%
% ?- middle(a, X).
% Resource error: insufficient memory
% (also query that never terminates?)
%
% --------VERSION 4--------
%
% ----Program----
% middle(X, [X]).
% middle(X, [H|T]) :-
%     middle(X, M),
%     append(M, [L], T).
%
% middle(X, [a, b, c]).
% X = b ? ; (seems it got one result)
% Resource error: insufficient memory
% (non-terminating query it seems???)
%
% middle(a, X).
% X = [a] ? ;
% X = [_A,a,_B] ? ;
% X = [_A,_B,a,_C,_D] ?
% (infinite results...)
