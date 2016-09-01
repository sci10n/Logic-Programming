count(V,G,S) :-
	findall(V,G,L),
	length(L,S).
