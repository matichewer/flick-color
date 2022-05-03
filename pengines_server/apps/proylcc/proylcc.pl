:- module(proylcc, 
	[  
		flick/5
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 
flick(Grid,X,Y,Color, NewGrid):- 
    		nth0(X,Grid,Lista),
    		reemplazarEnLista(Y, Lista, Color, NewList),
    		reemplazarEnLista(X, Grid, NewList, NewGrid).

% Dada una Lista, reemplaza el elemento por un NewElement. Retorna la NewList modificada
reemplazarEnLista(Pos, Lista, NewElement, NewList) :-
            nth0(Pos, Lista, _, R),
            nth0(Pos, NewList, NewElement, R).	


% obtener el color de una posicion
get(Grid,Px,Py,C):- nth0(Px,Grid,X), getY(X,Py,C).
getY(X,Py,C):- nth0(Py,X,C). 

% obtener adyacentes de las ESQUINAS
obtenerAdyacentes(0,0,L):-
    		insertar([1,0],[], L1),
    		insertar([0,1],L1, L).

obtenerAdyacentes(13,13,L):-
    		insertar([12,13],[], L1),
    		insertar([13,12],L1, L).

obtenerAdyacentes(0,13,L):-
    		insertar([0,12],[], L1),
    		insertar([1,13],L1, L).

obtenerAdyacentes(13,0,L):-
    		insertar([12,0],[], L1),
    		insertar([13,1],L1, L).
  
% obtener adyacentes de las PAREDES
obtenerAdyacentes(0,Y,L):-
    		Y>0, Y<13,
    		W is Y-1,
    		insertar([0, W],[], L1),
    		Z is Y+1,
    		insertar([0, Z],L1, L2),    		
    		insertar([1, Y],L2, L).

obtenerAdyacentes(X,0,L):-
    		X>0, X<13,
    		W is X-1,
    		insertar([W, 0],[], L1),
    		Z is X+1,
    		insertar([Z, 0],L1, L2),
    		insertar([1, X],L2, L).

obtenerAdyacentes(13,Y,L):-
    		Y>0, Y<13,
    		W is Y-1,
    		insertar([13, Y],[], L1),
    		insertar([13, W],L1, L2),
    		Z is Y+1,
    		insertar([12, Z],L2, L).

obtenerAdyacentes(X,13,L):-
    		X>0, X<13,
    		W is X-1,
    		insertar([W, 13],[], L1),
    		insertar([X, 12],L1, L2),
    		Z is X+1,
    		insertar([Z, 13],L2, L).

% obtener adyacentes GENERAL
obtenerAdyacentes(X, Y, L):-
    		X>0, X<13, Y>0, Y<13,
    		W is X-1,
    		insertar([W, Y],[], L1),
    		insertar([Y, W],L1, L2),
    		Z is X+1,
    		insertar([Z, Y],L2, L3),
    		insertar([Y, Z],L3, L).
    
	

% reemplaza el color en una grilla Grid, en la posición X,Y
% por el color pasado por parámetro.
% retorna la nueva grilla con el color modificado
replace(Grid,[X,Y],Color,NewGrid):- 
                  nth0(X,Grid, Fila), 
                  reemplazarElem(Y,Fila,Color,NewFila), % A REVISAR
                  reemplazarElem(X, Grid, NewFila, NewGrid).

reemplazarElem(Indice,Lista,Elem,R):-  
                  nth0(Indice,Lista,_,R1), 
                  nth0(Indice,R,Elem,R1).




% PREDICADOS PARA LA FINALIZACIÓN DEL JUEGO


% chequea que 2 colores sean iguales
mismoColor(X,X).

% dada una lista y un elemento X
% retorna true si todos los elementos son iguales al elemento X
%		  falso en caso contrario		 
todosIgualAlElementoX([], _X, true).
todosIgualAlElementoX([Elem|Lista], X, R):-
    					mismoColor(Elem,X),
    					todosIgualAlElementoX(Lista, X, R).

% recibe una lista
% retorna true si todos sus elementos son iguales
% 		  falso en contrario
todosLosElementosIguales([Elem1|Lista], R):-
    					nth0(1,Lista,Elem2), % obtengo segundo elemento
    					mismoColor(Elem1,Elem2), % los comparo
    					todosIgualAlElementoX(Lista, Elem1, R).

% chequea que todos los elementos de la grilla sean del mismo color
checkFinish(Grid, R):- 
    					nth0(0,Grid,L1),
    					todosLosElementosIguales(L1,R),
    					todosIgualAlElementoX(Grid,L1,R).


%  recibe la grilla y la lista de adyacentes
%  retorna la grilla con los colores actualizados
actualizarGrilla(Grid, Ady, NewGrid) :-


% 
adyacentes(Grid, [X,Y], ListaAdy) :-

% 
adyacentesC(X,Y) :-

adyacentesC*() :-

% longitud de adyacentesC
cantidadCapturados() :-


inicial(X,Y) :- assert(estado(X,Y,))







