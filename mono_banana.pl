%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% § Solución                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solucion(estado(_,_,_,con),[]).
solucion(E1,[A|L]) :-
   movimiento(E1,A,E2),
   solucion(E2,L).

% movimiento(estado(PM1,EM1,PS1,X1),A,estado(PM2,EM2,PS2,X2)) se
% verifica si en el estado(PM1,EM1,PS1,X1) se puede aplicar la acción A
% y se pasa al estado(PM2,EM2,PS2,X2) 
movimiento(estado(centro,silla,centro,sin),
           coger,
           estado(centro,silla,centro,con)).
movimiento(estado(X,suelo,X,U),
           subir,
           estado(X,silla,X,U)).
movimiento(estado(X1,suelo,X1,U),
           empujar(X1,X2),
           estado(X2,suelo,X2,U)).
movimiento(estado(X,suelo,Z,U),
           pasear(X,Z),
           estado(Z,suelo,Z,U)).