:- use_module(library(clpfd)).

container(a,2,2).
container(b,4,1).
container(c,2,2).
container(d,1,1).

on(a,d).
on(b,c).
on(c,d).

schedule(StartList, Sum, MaxEndTime, WorkersRequired) :-
	container_to_list(ContainerList),
 	length(ContainerList,N),
 	length(StartList,N),
 	length(EndList,N),
 	domain(StartList, 0, 100),
 	domain(EndList, 0, 100),
	WorkersRequired in 1..100,
	MaxEndTime in 1..100,
 	max_end_time(EndList,MaxEndTime),
 	check_constraints(StartList,EndList,ContainerList),
	task(StartList,EndList, ContainerList, Tasks),
 	cumulative(Tasks,[limit(WorkersRequired)]),
 	Sum #= WorkersRequired * MaxEndTime,
        labeling([minimize(Sum)], [Sum|StartList]).

max_end_time([],_).
max_end_time([End|EndList],MaxEndTime) :-
	MaxEndTime #>= End,
	max_end_time(EndList,MaxEndTime).

container_to_list(List) :- findall([Id,Workers,Duration], container(Id,Workers,Duration),List).

check_constraints([],[],[]).
check_constraints([Start|StartList],[End|EndList],[[Id,_,_]|ContainerList]) :-
	check_constraints2(Start,StartList,End,EndList,Id,ContainerList),
	check_constraints(StartList,EndList,ContainerList).
check_constraints(_,[],_,[],_,[]).

check_constraints2(Start1,[Start2|StartList],End1,[End2|EndList],Id1,[[Id2,_,_]|ContainerList]) :-
	constraint(Start1,Start2,End1,End2,Id1,Id2),
	check_constraints2(Start1,StartList,End1,EndList,Id1,ContainerList).

constraint(_, Start2, End1, _, Id1,Id2) :-
	on(Id1,Id2),
	Start2 #>= End1.
constraint(Start1, _, _, End2, Id1, Id2) :-
	on(Id2,Id1),
	Start1 #>= End2.
constraint(_, _, _, _, Id1, Id2) :-
	findall(_,on(Id1,Id2),[]),
	findall(_,on(Id2,Id1),[]).
	
task([],[],[],[]).
task([Start|StartList], [End|EndList], [[Id,Workers,Duration]|ContainerList], [T|TaskList]) :-
	T = task(Start,Duration,End,Workers,0),
	task(StartList,EndList,ContainerList,TaskList).
