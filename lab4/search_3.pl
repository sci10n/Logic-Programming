first(A:B, A).
second(A:B, B).

sub(A:B,C:B) :-
	C is A - 1.