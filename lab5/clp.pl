:- use_module(library(clpfd)).

container(a,2,2).
container(b,4,1).
container(c,2,2).
container(d,1,1).

on(a,d).
on(b,c).
on(c.d).

task(List) :-
%    Tasks = [task(_,2,_,2),
%             task(_4,_,1),
%             task(_,2,_2),
%             task(_,1,_,1)] 
    maplist(is_top,Tasks,List).
    
is_top(C) :-
    on(C,_).
