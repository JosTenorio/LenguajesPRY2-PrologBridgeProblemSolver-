:- dynamic(crossTime/2).
:- dynamic(timeLimit/1).
:- dynamic(crosserLimit/1).



%------------------Depth first call
depth_first_bridge(Sol) :-
      initial_state(State),      
      solve_dfs(State,[State],Sol).

%------------------Depth first solving
solve_dfs(State, _, []) :- 
      final_state(State).

solve_dfs(State,Path,[Move|Moves]) :-
      move(State, Move),
      update(State, Move, NewState),
      legal(NewState),
      not(member(NewState, Path)),
      solve_dfs(NewState, [State|Path], Moves).


%---------------Initial state
/*
initial_state([0,l,[a,b,c,d],[]]):-
      assert(timeLimit(17)),
      assert(crosserLimit(2)),
      assert(crossTime(a,1)),
      assert(crossTime(b,2)),
      assert(crossTime(c,5)),
      assert(crossTime(d,10)).
*/

prueba():-
      read(X),
      test(X).

test('Y'):-
      write('putosTodos').
test(_):-
      write('putos').


initial_state([0,l,L,[]]):-
      input_time_limit('N', 0),
      input_crossers('Y'),
      input_crosser_limit('N', 0),
      find_all_crossers([],L).

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


%---------------Final state
final_state([Time,r,[],L]):-
      timeLimit(X),
      Time =< X,
      find_all_crossers([],L).

find_all_crossers(Tmp,List):-
      (
         crossTime(NewName,_),
         not(member(NewName,Tmp)) ->
         find_all_crossers([NewName|Tmp],List); List = Tmp 
      ).

%------------Move
move([_,l,Left,_],Move):-
      cross(Left,Move).

move([_,r,_,Right],Move):-
      cross(Right,Move).

cross(Side,Move):- 
      crosserLimit(Limit),
      between(1,Limit,Size),
      comb(Size,Side,Move).

comb(N,L,X):-length(X,N),mem1(X,L).

mem1([],Y).
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



range(X, L, H) :- X is H - 1, X > L.
range(X, L, H) :- H1 is H - 1, H1 > L, range(X, L, H1).