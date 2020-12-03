:- dynamic(crossTime/2).
:- dynamic(timeLimit/1).


solve_dfs(Estado,_,[]) :- final_state(Estado).


solve_dfs(Estado,Historia,[Movida|Movidas]) :-
      move(Estado,Movida),              
      update(Estado,Movida,Estado2),     
      legal(Estado2),                    
      not(member(Estado2,Historia)),    
      solve_dfs(Estado2,[Estado2|Historia],Movidas).  

test_dfs(Problema,Movidas) :-
      initial_state(Estado),      
      solve_dfs(Estado,[Estado],Movidas).



initial_state([0,l,L,[]]):-
      find_all_crossers([],L).

final_state([X,r,[],L]):-
      timeLimit(X),
      find_all_crossers([],L).

find_all_crossers(Tmp,List):-
      (
         crossTime(NewName,_),
         not(member(NewName,Tmp)) ->
         find_all_crossers([NewName|Tmp],List); List = Tmp 
      ).


move(zgm(izq,I,_),Carga):-member(Carga,I).
move(zgm(der,_,D),Carga):-member(Carga,D).
move(zgm(_,_,_),solo).



update(zgm(B,I,D),Carga,zgm(B1,I1,D1)):-
      update_Bote(B,B1),                     
      update_margenes(Carga,B,I,D,I1,D1).   
                                             

update_Bote(izq,der).
update_Bote(der,izq).



update_margenes(solo,_,I,D,I,D). 


update_margenes(Carga,izq,I,D,I1,D1):-
      select(Carga,I,I1),      
      insert(Carga,D,D1).       


update_margenes(Carga,der,I,D,I1,D1):-
      select(Carga,D,D1),        
      insert(Carga,I,I1).        

insert(X,[Y|Ys],[X,Y|Ys]):-precedes(X,Y).  
insert(X,[Y|Ys],[Y|Zs]):-precedes(Y,X),insert(X,Ys,Zs). 
insert(X,[],[X]).                          

select(X,[X|Xs],Xs).                        
select(X,[Y|Ys],[Y|Zs]):-select(X,Ys,Zs).     

precedes(zorra,gallina).
precedes(zorra,maiz).
precedes(gallina,maiz).

legal(zgm(izq,_,D)):-not(ilegal(D)).
legal(zgm(der,I,_)):-not(ilegal(I)). 

ilegal(L):-member(zorra,L),member(gallina,L). 
ilegal(L):-member(gallina,L),member(maiz,L).  

