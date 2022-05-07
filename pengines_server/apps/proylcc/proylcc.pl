:- module(proylcc, 
	[  
		flick/6
	]).
:- dynamic(celdaCapturada/2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +X, +Y, +Color, -NewGrid,-CantidadCapturados)
% 
% NewGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla.
% En CantidadCapturados retorna la cantidad de celdas capturadas 
% En X,Y recibe las coordenadas de origen para el inicio del juego
flick(Grid,X,Y,Color,NewGrid,CantidadCapturados):- 
			assertCeldaCapturada(X,Y,_,_),
			getColor(Grid,X,Y,C1),
			Color \= C1,
			adyacentesC(Grid,Color,NewGrid),
			findall([A,B],celdaCapturada(A,B),ListaCapturados),
			length(ListaCapturados,CantidadCapturados).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INICIO ADYACENTES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% adyacentesC(+Grid,+Color,-NewGrid)
%
% Dada una grilla Grid, busca las celdas capturadas, 
% las pinta y captura las celdas adyacentes
adyacentesC(Grid,Color,NewGrid):-    		
			findall([X,Y],celdaCapturada(X,Y),CeldasCapturadas),
			pintarCapturadas(Grid,CeldasCapturadas,Color,NewGrid),
			obtenerAdyacentes(Grid,CeldasCapturadas,Color).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% obtenerAdyacentes(+Grid, +[[X|[Y]]|Celdas], +Color):-
%
% Dada una grilla Grid y (X,Y) las primeras coordenadas de la lista de Celdas
% Busca sus adyacentes del mismo color y los agrega a la lista
%
% Caso base:
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
obtenerAdyacentes(Grid,[[0|[Y]]|Celdas],Color):- % PARED ARRIBA
			Y>0, Y<13,
			W is Y-1,
			agregarCeldaCapturada(Grid,0,W,Color,Celdas,NewCeldas),
			Z is Y+1,
			agregarCeldaCapturada(Grid,0,Z,Color,NewCeldas,NewCeldas1),
			agregarCeldaCapturada(Grid,1,Y,Color,NewCeldas1,NewCeldas2),
			obtenerAdyacentes(Grid,NewCeldas2,Color).

obtenerAdyacentes(Grid,[[X|[0]]|Celdas],Color):- % PARED IZQUIERDA
			X>0, X<13,
			W is X-1,
			agregarCeldaCapturada(Grid,W,0,Color,Celdas,NewCeldas),
			Z is X+1,
			agregarCeldaCapturada(Grid,Z,0,Color,NewCeldas,NewCeldas1),
			agregarCeldaCapturada(Grid,X,1,Color,NewCeldas1,NewCeldas2),
			obtenerAdyacentes(Grid,NewCeldas2,Color).

obtenerAdyacentes(Grid,[[13|[Y]]|Celdas],Color):- % PARED ABAJO
			Y>0, Y<13,
			W is Y-1,
			agregarCeldaCapturada(Grid,13,W,Color,Celdas,NewCeldas),
			Z is Y+1,
			agregarCeldaCapturada(Grid,13,Z,Color,NewCeldas,NewCeldas1),
			agregarCeldaCapturada(Grid,12,Y,Color,NewCeldas1,NewCeldas2),
			obtenerAdyacentes(Grid,NewCeldas2,Color).

obtenerAdyacentes(Grid,[[X|[13]]|Celdas],Color):- % PARED DERECHA
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FIN DE ADYACENTES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% pintarCapturadas(+Grid, +[], +Color,-Grid)
% pintarCapturadas(+Grid, +[[X|[Y]]|Celdas], +Color, -NewGrid)
%
% Dada una grilla Grid, una lista de coordenadas X,Y de celdas capturadas, y un color
% Pinta todas las celdas capturadas y retorna la nueva grilla NewGrid
pintarCapturadas(Grid,[],_Color,Grid). 
pintarCapturadas(Grid,[[X|[Y]]|Celdas],Color, NewGrid):-
			reemplazarEnGrilla(Grid,X,Y,Color,NewGrid1),
			pintarCapturadas(NewGrid1,Celdas,Color,NewGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% agregarCeldaCapturada(+Grid, +X, +Y, +Color, +Celdas, -NewCeldas)
% agregarCeldaCapturada(_Grid,_X,_Y,_Color, +Celdas, -Celdas).
%
% Dada una grilla Grid, si en las coordenadas (X,Y) es del mismo color que el color recibido
% y no era una celda previamente capturada,
% Retorna la lista NewCeldas con las nueva celda capturada
agregarCeldaCapturada(Grid,X,Y,Color,Celdas,NewCeldas):-
			getColor(Grid,X,Y,C1),
			mismoColor(Color,C1),
			assertCeldaCapturada(X,Y,Celdas,NewCeldas).
% caso contrario devuelve la misma lista de celdas
agregarCeldaCapturada(_Grid,_X,_Y,_Color,Celdas,Celdas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% assertCeldaCapturada(+X,+Y,+Celdas,-NewCeldas):-
% assertCeldaCapturada(_,_,+Celdas,-Celdas).
%
% En caso de que una celda en las coordenadas (X,Y) aun no haya sido capturada,
% Se hace un assert de dicha celda y se agrega a la lista.
assertCeldaCapturada(X,Y,Celdas,NewCeldas):-
			not(celdaCapturada(X,Y)),    	
			insertarUltimo([X,Y],Celdas,NewCeldas),
			assert(celdaCapturada(X,Y)).
% Si la celda ya había sido capturada previamente, retorna la misma lista.
assertCeldaCapturada(_,_,Celdas,Celdas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% reemplazarEnGrilla(+Grid, +X, +Y, +NewElement, -NewGrid)
% 
% Dada una grilla Grid y una posicion (X,Y)
% Busca el elemento y lo reemplaza por NewElement
% Retorna la grilla NewGrid con el elemento reemplazado
reemplazarEnGrilla(Grid,X,Y,NewElement,NewGrid):- 
			nth0(X,Grid,Fila), 
			reemplazarEnLista(Y,Fila,NewElement,NewFila),
			reemplazarEnLista(X, Grid, NewFila, NewGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% reemplazarEnLista(+Indice, +Lista, +NewElement, -NewList)
% 
% Dada una Lista y un Indice,
% Busca el elemento y lo reemplaza por NewElement 
% Retorna una lista NewList con el elemento reemplazado
reemplazarEnLista(Indice, Lista, NewElement, NewList) :-
			nth0(Indice, Lista, _, R),
			nth0(Indice, NewList, NewElement, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% insertarEnLista(+E, +L, -[E|L]).
% 
% Inserta el elemento E al comienzo de la lista L
insertarEnLista(E,L,[E|L]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% insertarUltimo(+E, +[], -[E]).
% insertarUltimo(+E, +[L|Li], -[L|Zi])
% 
% Inserta el elemento E al final de la lista L
insertarUltimo(E,[],[E]).
insertarUltimo(E,[L|Li],[L|Zi]):-insertarUltimo(E,Li,Zi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getColor(+Grid,+X,+Y,-C)
% 
% Dada una grilla Grid, retorna el color C de una posicion (X,Y)
getColor(Grid,X,Y,C):- 
			nth0(X,Grid,Px),
			nth0(Y,Px,C). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% mismoColor(+X,+X).
% 
% Chequea que 2 colores sean iguales
mismoColor(X,X).
