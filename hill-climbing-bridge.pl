%------------------Dynamic statements
:- dynamic(crossTime/2).
:- dynamic(timeLimit/1).
:- dynamic(crosserLimit/1).
:- dynamic(totalCrossingTimes/1).

%------------------Hill Climbing call
hill_climbing_bridge() :-
    initial_state(State),           
    solve_hill_climb(State,[State],Moves), 
    nl,
    write('Possible solution (moves): '),
    write(Moves),nl.

solve_hill_climb(State,_,[]) :-
    final_state(State).

solve_hill_climb(State,History,[Move|Moves]) :-
    hill_climb(State,Move),      
    update(State,Move,State1),   
    legal(State1),               
    not(member(State1,History)), 
    solve_hill_climb(State1,[State1|History],Moves).   

%------------------Hill Climbing solving
hill_climb(State,Move) :-
    findall(M,move(State,M),Moves),         
    evaluate_and_order(Moves,State,[],MVs), 
    member((Move,_),MVs).                   

evaluate_and_order([Move|Moves],State,MVs,OrderedMVs) :-    
    value(State,Move,Value),               
    insertPair((Move,Value),MVs,MVs1), 
    evaluate_and_order(Moves,State,MVs1,OrderedMVs).  
    
evaluate_and_order([],_,MVs,MVs).

insertPair(MV,[],[MV]).
insertPair((M,V),[(M1,V1)|MVs],[(M,V),(M1,V1)|MVs]) :-
    V >= V1.
insertPair((M,V),[(M1,V1)|MVs],[(M1,V1)|MVs1]) :-
    V < V1,insertPair((M,V),MVs,MVs1).

value([_,r,_,_],Move,Value):-
    crossing_times(Move, Times),
    sum_list(Times,Time_sum),
    totalCrossingTimes(Total),
    Max is Total + 1,
    Value is Max - Time_sum.


value([_,l,_,[]],Move,Value):-
    crossing_times(Move, Times),
    sum_list(Times,Time_sum),
    totalCrossingTimes(Total),
    Max is Total + 1,
    Value is Max - Time_sum.

value([_,l,L,R],Move,Value):-


crossing_times([], 0).
crossing_times(Crossers, L1) :-
    findall(Y,(crossTime(X,Y),member(X,Crossers)),L1),

sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.

%---------------Parameters
%Through dynamic statements, it is possible to avoid user input of parameters
timeLimit(30).
crosserLimit(3).
crossTime(alberto,1).
crossTime(beatriz,2).
crossTime(carlos,5).
crossTime(dora,10).
crossTime(emilio,15).
crossTime(julio,15).

%The following code allows for user input of the parameters
/*
initial_state([0,l,L,[]]):-
      input_time_limit('N', 0),
      input_crossers('Y'),
      input_crosser_limit('N', 0),
      find_all_crossers([],L),
      sum_croosing_times(L,Times),
      assert(totalCrossingTimes(Times)).

input_time_limit('Y',Time):-
      assert(timeLimit(Time)).

input_time_limit('N',_):-
    write('Digite el tiempo limite en minutos: '),nl,
    read(Time),
    write('Es este tiempo: '), 
    write(Time),write(', correcto? (Y/N)'),nl,
    read(Response),
    input_time_limit(Response,Time).

input_crosser_limit('Y',Limit):-
      assert(crosserLimit(Limit)).

input_crosser_limit('N',_):-
    write('Digite la capacidad limite del puente: '),nl,
    read(Limit),
    write('Es esta capacidad: '), 
    write(Limit),write(', correcta? (Y/N)'),nl,
    read(Response),
    input_crosser_limit(Response,Limit).

input_crossers('Y') :-
      write('Inserte el nombre de una persona:'),nl,
      read(Name),
      write('Inserte el tiempo que tarda en cruzar el puente:'),nl,
      read(Time),
      assert_crossers(Name,Time),
      write('Desea ingresar otra persona? (Y/N):'),nl,
      read(X),
      input_crossers(X).

input_crossers('N').

assert_crossers(Name, Time):-
      assert(crossTime(Name, Time)).
*/


%---------------Initial state

initial_state([0,l,L,[]]):-
    findall(X,crossTime(X,_),L),
    crossing_times(L, Times),
    sum_list(Times,Time_sum),
    assert(totalCrossingTimes(Time_sum)).

%---------------Final state
final_state([_,r,[],L2]):-
      findall(X,crossTime(X,_),L1),
      isSubset(L2,L1).

isSubset([],_).
isSubset([H|T],Y):-
    member(H,Y),
    select(H,Y,Z),
    isSubset(T,Z).
equal(X,Y):-
    isSubset(X,Y),
    isSubset(Y,X).

%------------Move
move([_,l,Left,_],Move):-
      cross(l,Left,Move).

move([_,r,_,Right],Move):-
      cross(r,Right,Move).

%This version of cross cuts solutions in which more tan one person returns to the left side from the right side anytime.
cross(l,Crossers,Move):-
      crosserLimit(Limit),
      Limit2 is Limit + 1,
      range(Size,0,Limit2),
      comb(Size,Crossers,Move).
cross(r,Crossers,Move):-
      comb(1,Crossers,Move).

%This version of cross includes solutions in which more tan one person returns to the left side from the right side anytime,
%but it makes the program considerably slower.
/*
cross(l,Crossers,Move):-
      crosserLimit(Limit),
      Limit2 is Limit + 1,
      range(Size,0,Limit2),
      comb(Size,Crossers,Move).
cross(r,Crossers,Move):-
      crosserLimit(Limit),
      between(1,Limit,Size),
      comb(Size,Crossers,Move).
*/

range(X, L, H) :- X is H - 1, X > L.
range(X, L, H) :- H1 is H - 1, H1 > L, range(X, L, H1).

comb(N,L,X):-length(X,N),mem1(X,L).

mem1([],_).
mem1([H|T],Y):-member(H,Y),rest(H,Y,New),mem1(T,New).

rest(A,L,R):- append(_,[A|R],L),!.

%------------Update
update([T1,l,L1,R1], Move,[T2,r,L2,R2]):-
      take(Move,L1,L2),
      append(Move,R1,R2),
      findtime(Move,T),
      T2 is T1+T.

update([T1,r,L1,R1], Move,[T2,l,L2,R2]):-
      take(Move,R1,R2),
      append(Move,L1,L2),
      findtime(Move,T),
      T2 is T1+T.

take(S,L,R):- findall(Z,(member(Z,L),not(member(Z,S))),R).


findtime([P|T], O) :-
      crossTime(P,Time),
      findtime(T, Time, O).

findtime([], P, P).
findtime([H|T], P, O) :-
    crossTime(H,Time),
    (    Time > P
    ->   findtime(T, Time, O)
    ;    findtime(T, P, O)).

%---------------Legal
legal([Time, _, _, _]) :-
    timeLimit(Limit),
    Time =< Limit.