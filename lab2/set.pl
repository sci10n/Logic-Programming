% set/1 - Are the given elements one set?
set([]). % Empty set is trivially sorted.
set([_]). % Handle the single edge case.
set([H1,H2|T]) :- H1 @< H2, set([H2|T]).

% union/3 - Union between two given sets.
union([], [], []). % Handle the trivial case.
% The clauses below will expand A, and B to also:
% A = [] and B = [] which will lead to duplicate.
union(A,[],A) :- set(A), A \== []. % See above...
union([],B,B) :- set(B), B \== []. % See above...
union([A|LeftSet], [B|RightSet], [A|SetUnion]) :-
    union(LeftSet, [B|RightSet], SetUnion),
    set([A|LeftSet]), set([B|RightSet]),
    A @< B.
union([A|LeftSet], [B|RightSet], [B|SetUnion]) :-
    union([A|LeftSet], RightSet, SetUnion),
    set([A|LeftSet]), set([B|RightSet]),
    A @> B.
union([A|LeftSet], [A|RightSet], [A|SetUnion]) :-
    union(LeftSet, RightSet, SetUnion),
    set([A|LeftSet]), set([A|RightSet]).

% intersection/3 - Simple set intersection.
intersection([], [], []). % The base case...
intersection(A, [], []) :- set(A), A \== [].
intersection([], B, []) :- set(B), B \== [].
intersection([L|LeftSet], [R|RightSet], SetIntersection) :-
    intersection([L|LeftSet], RightSet, SetIntersection),
    set([L|LeftSet]), set([R|RightSet]), % Only on sets.
    L @> R.
intersection([L|LeftSet], [R|RightSet], SetIntersection) :-
    intersection(LeftSet, [R|RightSet], SetIntersection),
    set([L|LeftSet]), set([R|RightSet]), % Only on sets.
    L @< R.
intersection([L|LeftSet], [L|RightSet], [L|SetIntersection]) :-
    intersection(LeftSet, RightSet, SetIntersection),
    set([L|LeftSet]), set([L|RightSet]).

% power_set/2 - Produces the power set of one predefined set.
power_set([], [[]]). % Per the definition of the power set it has the zero set as well.
power_set(Set, PowerSet) :- findall(X, (union(X, Y, Set), intersection(X, Y, [])), PowerSet).
% Find all combinations we can arrange X, Y via the union, but also removing any duplicates.

% -------------------------
% ------EXAMPLE QUERY------
% -------------------------
%
% Is [a, b, c] a valid set?
% -------------------------
%
% ?- set([a, b, c]).
% yes
%
% Is [c, b, a] a valid set?
% -------------------------
%
% ?- set([c, b, a]).
% no
%
% What is the union of [a, b, c] and [d, e, f]?
% ---------------------------------------------
%
% ?- union([a, b, c], [d, e, f], S).
% S = [a,b,c,d,e,f] ? ;
% no
%
% What is the union of [a, b, c, d] and [d, e, f]?
% ------------------------------------------------
%
% ?- union([a, b, c, d], [d, e, f], S).
% S = [a,b,c,d,e,f] ? ;
% no
%
% Can we do a union of [c, b, a] and [d, e, f]?
% ---------------------------------------------
%
% ?- union([c, b, a], [d, e, f], S).
% no
%
% What is the intersection of [a, b, c] and [c, d, e]?
% ----------------------------------------------------
% ?- intersection([a, b, c], [c, d, e], S).
% S = [c] ? ;
% no
%
% What is the power set of the set [a, b, c]?
% -------------------------------------------
% ?- power_set([a, b, c], S).
% S = [[a,b,c],[],[a],[a,b],[a,c],[b,c],[b],[c]]
% no
