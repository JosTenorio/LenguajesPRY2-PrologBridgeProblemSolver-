%------------------Dynamic statements
:- dynamic(crossTime/2).
:- dynamic(timeLimit/1).
:- dynamic(crosserLimit/1).
:- dynamic(totalCrossingTimes/1).
:- dynamic(top_1_crossers/1).
:- dynamic(top_2_crossers/1).

%---------------Parameters
%Through dynamic statements, it is possible to avoid user input of parameters
timeLimit(28).
crosserLimit(2).
crossTime(alberto,1).
crossTime(beatriz,2).
crossTime(carlos,5).
crossTime(dora,10).
crossTime(emilio,15).
%crossTime(julio,20).


%----------------Best First call
test_best_search() :-
   initial_state(State),    
   solve_best([punto(State,[],0)],[State],Sol),
   nl,
   write('Possible solution (moves): '),
   write(Sol),nl.



%----------------Best First solving
solve_best([punto(State,Path,_)|_],_,Moves) :-
    final_state(State),reverse(Path,Moves).
solve_best([punto(State,Path,_)|Frontier],History,FinalPath) :-
    findall(M,move(State,M),Moves),     
    updates(Moves,Path,State,States),   
    legals(States,States1),            
    news(States1,History,States2),     
    evaluates(States2,Values),          
    inserts(Values,Frontier,Frontier1), 
    solve_best(Frontier1,[State|History],FinalPath). 

updates([M|Ms],Path,S,[(S1,[M|Path])|Ss]) :-
    update(S,M,S1),         
    updates(Ms,Path,S,Ss).  
updates([],_,_,[]).

legals([(S,P)|States],[(S,P)|States1]) :-
    legal(S),
    legals(States,States1).
legals([(S,_)|States],States1) :-
    not(legal(S)),
    legals(States,States1). 
legals([],[]).


news([(S,_)|States],History,States1) :-
    member(S,History),
    news(States,History,States1).
news([(S,P)|States],History,[(S,P)|States1]) :-
    not(member(S,History)),
    news(States,History,States1). 
news([],_,[]).

evaluates([(S,P)|States],[punto(S,P,V)|Values]) :-
    value(S,V),                
    evaluates(States,Values).  
evaluates([],[]).


inserts([Punto|Puntos],Frontier,Frontier1) :-
    insertPoint(Punto,Frontier,Frontier0), 
    inserts(Puntos,Frontier0,Frontier1).    
inserts([],Frontier,Frontier).


insertPoint(Point,[],[Point]).
insertPoint(Point,[Point1|Points],[Point1,Point|Points]) :-
    less_than(Point1,Point).
insertPoint(Point,[Point1|Points],[Point|Points]) :-
    equals(Point,Point1).
insertPoint(Point,[Point1|Points],[Point1|Points1]) :-
    less_than(Point,Point1),
    insertPoint(Point,Points,Points1).
insertPoint(Point,[Point1|Points],[Point,Point1|Points]) :-
    same(Point,Point1).

equals(punto(S,_,V),punto(S,_,V)).

less_than(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 < V2.

same(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 = V2.

%---------------Value
%When the torch came from the right, assign better values to the fastest crossers.
value([Time, l, _, _], Value):-
    totalCrossingTimes(X),
    Max is X + 1,
    Value is Max - Time.

%When the torch was on the left and the top  1 crossers and the top 2 crossers are now on the same side,
%assign better worse adjustment value and favor bigger groups
value([_, r, L, R], Value):-
    top_1_crossers(Top_1_crossers),
    top_2_crossers(Top_2_crossers),
    append(Top_1_crossers,Top_2_crossers,Top_crossers),
    (isSubset(Top_crossers,L); isSubset(Top_crossers,R)),
    length(R, Len),
    Value is 1 + (Len*50).

%When the torch was on the left and the top 1 crossers and the top 2 crossers are on different sides,
%assign better better adjustment value and favor bigger groups
value([_, r, L, R], Value):-
    top_1_crossers(Top_1_crossers),
    top_2_crossers(Top_2_crossers),
    append(Top_1_crossers,Top_2_crossers,Top_crossers),
    not(isSubset(Top_crossers,L); isSubset(Top_crossers,R)),
    length(R, Len),
    Value is 2 + (Len*50).

sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.

crossing_times([], 0).
crossing_times(Crossers, L1):-
    findall(Y,(crossTime(X,Y),member(X,Crossers)),L1).

%---------------Initial state

initial_state([0,l,L,[]]):-
    findall(X,crossTime(X,_),L),
    total_times(L,TotalCrossingTimes),
    top_crossers(L,Top_1_crossers,Top_2_crossers),
    assert(top_1_crossers(Top_1_crossers)),
    assert(top_2_crossers(Top_2_crossers)),  
    assert(totalCrossingTimes(TotalCrossingTimes)).

total_times(L,TotalCrossingTimes):-
    crossing_times(L, Times),
    sum_list(Times,TotalCrossingTimes).

top_crossers(L,Top_1_crossers,Top_2_crossers):-
    crossing_times(L, Times),
    min_list(Times,Top_1_time),
    findall(X,crossTime(X,Top_1_time),Top_1_crossers),
    delMember(Top_1_time,Times,Times2),
    min_list(Times2,Top_2_time),
    findall(X,crossTime(X,Top_2_time),Top_2_crossers).


delMember(X, [], []) :- !.
delMember(X, [X|Xs], Y) :- !, delMember(X, Xs, Y).
delMember(X, [T|Xs], Y) :- !, delMember(X, Xs, Y2), append([T], Y2, Y).


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
