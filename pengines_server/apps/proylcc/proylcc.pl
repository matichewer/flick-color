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


% setear color a una posicion especifica
setColor(Grid,X,Y,Color) :-

%
mismoColor(X,X).

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


% COMMIT EN CLASE DIA 29-04-2022
% consulta de nth/3
% devuelve la fila 1
nth0(1,[
		 [y,g,b,g,v,y,p,v,b,p,v,p,v,r],
		 [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
		 [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
		 [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
		 [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
		 [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
		 [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
		 [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
		 [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
		 [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
		 [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
		 [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
		 [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
		 [v,g,p,b,v,v,g,g,g,b,v,g,g,g]
		 ], X).

