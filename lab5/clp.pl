:- use_module(library(clpfd)).

container(a,2,2).
container(b,4,1).
container(c,2,2).
container(d,1,1).

on(a,d).
on(b,c).
on(c,d).

schedule(Sum) :-
 	on_to_list(OnList),
	container_to_list(ContainerList),
 	length(ContainerList,N),
 	length(StartList,N),
 	length(EndList,N),
 	domain(StartList, 0, 100),
    domain(EndList, 0, 100),
 	task(StartList,EndList, ContainerList, Tasks),
 	WorkersRequired in 1..100,
 	cumulative(Tasks,[limit(WorkersRequired)]),
 	max_end_time(EndList,MaxEndTime),
 	Sum #= WorkersRequired * MaxEndTime.

max_end_time([],_).
max_end_time([End|EndList],MaxEndTime) :-
	MaxEndTime #>= End,
	max_end_time(EndList,MaxEndTime).

container_to_list(List) :- findall([Id,Workers,Duration], container(Id,Workers,Duration),List).
on_to_list(List) :- findall([X,Y],on(X,Y),List).

on_top(X,[]).
on_top(X,[[_,T]|List]) :-
	dif(X,T),
	on_top(X,List).

constraint(Id,Workers,Time,ContraintList) :- 
	on_top(Id,ContraintList).
constraint(Id,Workers,Time) :- 
	container(Id,RequiredWorkers, RequiredTime),
	Workers #>= RequiredWorkers.

task([],[],[],[]).
task([Start|StartList], [End|EndList], [[Id,Workers,Duration]|ContainerList], [T|TaskList]) :-
	T = task(Start,Duration,End,Workers,0),
	task(StartList,EndList,ContainerList,TaskList).
