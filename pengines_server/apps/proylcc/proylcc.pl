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

% dada una Lista y un Indice,
% busca el elemento y lo reemplaza por NewElement 
% retorna una lista NewList con el elemento reemplazado
reemplazarEnLista(Indice, Lista, NewElement, NewList) :-
            nth0(Indice, Lista, _, R),
            nth0(Indice, NewList, NewElement, R).	

% dada una grilla Grid y una posicion (X,Y)
% busca el color y lo reemplaza por NewElement
% retorna la grilla NewGrid con el elemento reemplazado
reemplazarEnGrilla(Grid,[X,Y],NewElement,NewGrid):- 
			nth0(X,Grid,Fila), 
			reemplazarEnLista(Y,Fila,NewElement,NewFila),
			reemplazarEnLista(X, Grid, NewFila, NewGrid).

% inserta el elemento E al comienzo de la lista L
insertarEnLista(E,L,[E|L]).

% dada una grilla Grid, obtiene el color C de una posicion (X,Y)
getColor(Grid,X,Y,C):- 
    		nth0(X,Grid,Px),
    		nth0(Y,Px,C). 

% obtener adyacentes de las ESQUINAS
obtenerAdyacentes(Grid,[[0|[0]]|Celdas],Color):-
    		agregarCeldaCapturada(Grid,0,1,Color),
    		agregarCeldaCapturada(Grid,1,0,Color).

% obtener adyacentes de las ESQUINAS
obtenerAdyacentes(Grid,[[13|[13]]|Celdas],Color):-
    		agregarCeldaCapturada(Grid,12,13,Color),
    		agregarCeldaCapturada(Grid,13,12,Color).
/*
obtenerAdyacentes(0,13,L):-
    		insertarEnLista([0,12],[], L1),
    		insertar([1,13],L1, L).

obtenerAdyacentes(13,0,L):-
    		insertarEnLista([12,0],[], L1),
    		insertarEnLista([13,1],L1, L).
  
% obtener adyacentes de las PAREDES
obtenerAdyacentes(0,Y,L):-
    		Y>0, Y<13,
    		W is Y-1,
    		insertarEnLista([0, W],[], L1),
    		Z is Y+1,
    		insertarEnLista([0, Z],L1, L2),    		
    		insertarEnLista([1, Y],L2, L).

obtenerAdyacentes(X,0,L):-
    		X>0, X<13,
    		W is X-1,
    		insertarEnLista([W, 0],[], L1),
    		Z is X+1,
    		insertarEnLista([Z, 0],L1, L2),
    		insertarEnLista([1, X],L2, L).

obtenerAdyacentes(13,Y,L):-
    		Y>0, Y<13,
    		W is Y-1,
    		insertarEnLista([13, Y],[], L1),
    		insertarEnLista([13, W],L1, L2),
    		Z is Y+1,
    		insertarEnLista([12, Z],L2, L).

obtenerAdyacentes(X,13,L):-
    		X>0, X<13,
    		W is X-1,
    		insertarEnLista([W, 13],[], L1),
    		insertarEnLista([X, 12],L1, L2),
    		Z is X+1,
    		insertarEnLista([Z, 13],L2, L).

% obtener adyacentes GENERAL
obtenerAdyacentes(X, Y, L):-
    		X>0, X<13, Y>0, Y<13,
    		W is X-1,
    		insertarEnLista([W, Y],[], L1),
    		insertarEnLista([Y, W],L1, L2),
    		Z is X+1,
    		insertarEnLista([Z, Y],L2, L3),
    		insertarEnLista([Y, Z],L3, L).
    
*/
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% INICIO PREDICADOS PARA LA FINALIZACIÓN DEL JUEGO %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% FIN PREDICADOS PARA LA FINALIZACIÓN DEL JUEGO %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





% DE ACÁ PARA ABAJO SON LOS PREDICADOS QUE EN 
% UN PRINCIPIO PENSAMOS CON NUESTRA SUPER INTELIGENCIA

%____________________________________________________________________
% Pinta todas las celdas capturadas
pintarCapturadas(Grid,[],_Color,Grid).
pintarCapturadas(Grid,[[X|[Y]]|Celdas],Color, NewGrid):-
    	%getColor(Grid,X,Y,C),  % ¿NO CONTROLAMOS MISMO COLOR?
    	%C \= Color,
    	reemplazarEnGrilla(Grid,X,Y,Color,NewGrid1),
		pintarCapturadas(NewGrid1,Celdas,Color,NewGrid).
% su consulta:
%init1(Grid),
%findall([X,Y],celdaCapturada(X,Y),CeldasCapturadas),
%pintarCapturadas(Grid,CeldasCapturadas,r,NewGrid).
%____________________________________________________________________

% en caso de que no exista la celda capturada, 
% se hace un assert de dicha celda.
assertCeldaCapturada(X,Y):-
    	not(celdaCapturada(X,Y)),
    	assert(celdaCapturada(X,Y)).
assertCeldaCapturada(_,_).
%____________________________________________________________________

% dada una grilla G, si en las coordenadas (X,Y) está el color C
% entonces se agrega dichas coordenadas a la lista L
% y se retorna en R
insertarCoordEnListaSiEsDelMismoColor(G,X,Y,C,L,ListaAdy):-
    		getColor(G,X,Y,C1),
    		mismoColor(C,C1),
    		insertarEnLista([X,Y],L,ListaAdy).% ¿AGREGAMOS COORDS REPETIDAS??
% dada una grilla G, si en las coordenadas (X,Y) NO está el color C
% entonces se retorna la misma lista recibida
insertarCoordEnListaSiEsDelMismoColor(_G,_X,_Y,_C,L,L).
%____________________________________________________________________
agregarCeldaCapturada(Grid,X,Y,Color):-
    		getColor(Grid,X,Y,C1),
    		mismoColor(Color,C1),
    		assertCeldaCapturada(X,Y).
agregarCeldaCapturada(_G,_X,_Y,_Color).
%____________________________________________________________________
%adyacente(Grid, Color):-
    		% pintar celdas capturas de findall
    		% 
    		% para cada celda capturada
    			% buscar adyacentes mismo color
    			% agregar nuevas celdas capturadas sin repetir
celdaCapturada(13,13).

adyacentesC(Grid,Color,NewGrid):-    		
    	% pintamos todas las celdas capturadas
          findall([X,Y],celdaCapturada(X,Y),CeldasCapturadas),
          pintarCapturadas(Grid,CeldasCapturadas,Color,NewGrid),
		  % obtenerAdyacentes y agregarlos como celdas capturadas%
		 obtenerAdyacentes(Grid,CeldasCapturadas,Color).
%____________________________________________________________________



% COSAS FEAS:

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









