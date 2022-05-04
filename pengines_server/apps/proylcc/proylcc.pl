:- module(proylcc, 
	[  
		flick/7
	]).
	:- dynamic(celdaCapturada/2).

celdaCapturada(6,3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 
flick(Grid,X,Y,Color,NewGrid,CantidadCapturados,Complete):- 
			%assertCeldaCapturada(X,Y,_,_), % HAY QUE REVISAR ÉSTA LINEA
			adyacentesC(Grid,Color,NewGrid),
			findall([A,B],celdaCapturada(A,B),ListaCapturados),
			length(ListaCapturados,CantidadCapturados).
			termino(CantidadCapturados,Complete).

% verifica que el juego haya terminado
%termino(196,true).
%termino(_,false).
termino(Cant,Fin):-
    		Cant = 196,
    		Fin is 0.

termino(Cant, Fin):-
    		Cant \= 196,
    		Fin is 1.


% dada una Lista y un Indice,
% busca el elemento y lo reemplaza por NewElement 
% retorna una lista NewList con el elemento reemplazado
reemplazarEnLista(Indice, Lista, NewElement, NewList) :-
            nth0(Indice, Lista, _, R),
            nth0(Indice, NewList, NewElement, R).

% dada una grilla Grid y una posicion (X,Y)
% busca el color y lo reemplaza por NewElement
% retorna la grilla NewGrid con el elemento reemplazado
reemplazarEnGrilla(Grid,X,Y,NewElement,NewGrid):- 
			nth0(X,Grid,Fila), 
			reemplazarEnLista(Y,Fila,NewElement,NewFila), % A REVISAR (?)
			reemplazarEnLista(X, Grid, NewFila, NewGrid).


% inserta el elemento E al comienzo de la lista L
insertarEnLista(E,L,[E|L]).

% dada una grilla Grid, obtiene el color C de una posicion (X,Y)
getColor(Grid,X,Y,C):- 
    		nth0(X,Grid,Px),
    		nth0(Y,Px,C). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% INICIO ADYACENTES %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adyacentesC(Grid,Color,NewGrid):-    		
    	% pintamos todas las celdas capturadas
          findall([X,Y],celdaCapturada(X,Y),CeldasCapturadas),
          pintarCapturadas(Grid,CeldasCapturadas,Color,NewGrid),
		  % obtenerAdyacentes y agregarlos como celdas capturadas%
		 obtenerAdyacentes(Grid,CeldasCapturadas,Color).

% CASO BASE
obtenerAdyacentes(_Grid,[],_Color).

% obtener adyacentes de las ESQUINAS
obtenerAdyacentes(Grid,[[0|[0]]|Celdas],Color):-
    		agregarCeldaCapturada(Grid,0,1,Color,Celdas,NewCeldas),
    		agregarCeldaCapturada(Grid,1,0,Color,NewCeldas,NewCeldas1),
			obtenerAdyacentes(Grid,NewCeldas1,Color).

obtenerAdyacentes(Grid,[[13|[13]]|Celdas],Color):-
    		agregarCeldaCapturada(Grid,12,13,Color,Celdas,NewCeldas),
    		agregarCeldaCapturada(Grid,13,12,Color,NewCeldas,NewCeldas1),
			obtenerAdyacentes(Grid,NewCeldas1,Color).
    		
obtenerAdyacentes(Grid,[[0|[13]]|Celdas],Color):-
    		agregarCeldaCapturada(Grid,0,12,Color,Celdas,NewCeldas),
    		agregarCeldaCapturada(Grid,1,13,Color,NewCeldas,NewCeldas1),
			obtenerAdyacentes(Grid,NewCeldas1,Color).

obtenerAdyacentes(Grid,[[13|[0]]|Celdas],Color):-
    		agregarCeldaCapturada(Grid,12,0,Color,Celdas,NewCeldas),
    		agregarCeldaCapturada(Grid,13,1,Color,NewCeldas,NewCeldas1),
			obtenerAdyacentes(Grid,NewCeldas1,Color).

% obtener adyacentes de las PAREDES
obtenerAdyacentes(Grid,[[0|[Y]]|Celdas],Color):-
    		Y>0, Y<13,
    		W is Y-1,
    		agregarCeldaCapturada(Grid,0,W,Color,Celdas,NewCeldas),
    		Z is Y+1,
    		agregarCeldaCapturada(Grid,0,Z,Color,NewCeldas,NewCeldas1),
    		agregarCeldaCapturada(Grid,1,Y,Color,NewCeldas1,NewCeldas2),
			obtenerAdyacentes(Grid,NewCeldas2,Color).

obtenerAdyacentes(Grid,[[X|[0]]|Celdas],Color):-
    		X>0, X<13,
    		W is X-1,
    		agregarCeldaCapturada(Grid,W,0,Color,Celdas,NewCeldas),
    		Z is X+1,
    		agregarCeldaCapturada(Grid,Z,0,Color,NewCeldas,NewCeldas1),
    		agregarCeldaCapturada(Grid,1,X,Color,NewCeldas1,NewCeldas2),
			obtenerAdyacentes(Grid,NewCeldas2,Color).

obtenerAdyacentes(Grid,[[13|[Y]]|Celdas],Color):-
    		Y>0, Y<13,
    		W is Y-1,
    		agregarCeldaCapturada(Grid,13,Y,Color,Celdas,NewCeldas),
    		agregarCeldaCapturada(Grid,13,W,Color,NewCeldas,NewCeldas1),
    		Z is Y+1,
    		agregarCeldaCapturada(Grid,12,Z,Color,NewCeldas1,NewCeldas2),
			obtenerAdyacentes(Grid,NewCeldas2,Color).

obtenerAdyacentes(Grid,[[X|[13]]|Celdas],Color):-
    		X>0, X<13,
    		W is X-1,
    		agregarCeldaCapturada(Grid,W,13,Color,Celdas,NewCeldas),
    		agregarCeldaCapturada(Grid,X,12,Color,NewCeldas,NewCeldas1),
    		Z is X+1,
    		agregarCeldaCapturada(Grid,Z,13,Color,NewCeldas1,NewCeldas2),
			obtenerAdyacentes(Grid,NewCeldas2,Color).

% CASO GENERAL
obtenerAdyacentes(Grid,[[X|[Y]]|Celdas],Color):-
    		X>0, X<13, Y>0, Y<13,
    		W is X-1, 
    		agregarCeldaCapturada(Grid,W,Y,Color,Celdas,NewCeldas), % ARRIBA
    		Z is X+1,
    		agregarCeldaCapturada(Grid,Z,Y,Color,NewCeldas,NewCeldas1), % ABAJO
    		A is Y-1,    
    		agregarCeldaCapturada(Grid,X,A,Color,NewCeldas1,NewCeldas2), % IZQUIERDA
    		B is Y + 1,
    		agregarCeldaCapturada(Grid,X,B,Color,NewCeldas2,NewCeldas3), % DERECHA
			obtenerAdyacentes(Grid,NewCeldas3,Color).

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
assertCeldaCapturada(X,Y,Celdas,NewCeldas):-
    	not(celdaCapturada(X,Y)),    	
    	insertarUltimo([X,Y],Celdas,NewCeldas),
    	assert(celdaCapturada(X,Y)).
assertCeldaCapturada(_,_,Celdas,Celdas).
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
agregarCeldaCapturada(Grid,X,Y,Color,Celdas,NewCeldas):-
    		getColor(Grid,X,Y,C1),
    		mismoColor(Color,C1),
    		assertCeldaCapturada(X,Y,Celdas,NewCeldas).
agregarCeldaCapturada(_G,_X,_Y,_Color,Celdas,Celdas).
%____________________________________________________________________
insertarUltimo(E,[],[E]).
insertarUltimo(E,[L|Li],[L|Zi]):-insertarUltimo(E,Li,Zi).

% chequea que 2 colores sean iguales
mismoColor(X,X).


/*	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% INICIO PREDICADOS PARA LA FINALIZACIÓN DEL JUEGO %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

*/


/* COSAS FEAS:

% longitud de adyacentesC
cantidadCapturados() :-

inicial(X,Y) :- assert(estado(X,Y,)).

*/






