:- module(proylcc, 
	[  
		flick/3
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 

flick(Grid, Color, FGrid):-
	Grid = [F|Fs],
	F = [X|Xs],
	Color \= X,
	FGrid = [[Color|Xs]|Fs].


% obtener el color de una posicion
getPosicion(Grid,X,Y, Color) :- 


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

