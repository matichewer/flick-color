:- module(proylcc, [flick/8]).

% El hecho ganador/2 lo usamos para guardar cada puntuacion ganadora
% Ejemplo: ganador(mati,25).
:- dynamic ganador/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% botonAyuda( +Grid, +Origen, +ListaCapturados, +Profundidad, -SecuenciaColores, -CantidadAdyacentes)
%
% Grid es la grilla actual
% Origen es la celda origen en formato [X,Y]
% ListaCapturados es la lista actual de celdas capturadas
% Profundidad es la profunidad que solicita el usuario
% SecuenciaColores es la secuencia de colores que le sugerimos al usuario
% CantidadAdyacentes es la cantidas de celdas capturadas luego de realizar la secuencia de colores
%
botonAyuda(Grid, Origen, ListaCapturados, Profundidad, SecuenciaColores, CantidadAdyacentes):-    
		estrategia([ [Grid,Origen,ListaCapturados,[],0] ], Profundidad, TodosLosResultados),		
		mejorResultado(TodosLosResultados, R),
		R = [_Grid,_Origen,NewListaCapturados,SecuenciaColores, _ProfInicial],
		length(NewListaCapturados,CantidadAdyacentes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilizamos un bloque de información el cual consiste en una
% lista 5 variables, la cual tiene el siguiente formato:
%
% 	[Grilla, Origen, ListaCapturados, SecuenciaColores, ProfundidadActual]
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% estrategia([ +[Grid,Origen,ListaCapturados,SecuenciaColores, ProfunidadActual] ], +ProfundidadFija, -NewResultados)
%
% Grid es la grilla actual
% Origen es la celda origen en formato [X,Y]
% ListaCapturados es la lista actual de celdas capturadas
% SecuenciaColores es la secuencia de colores que vamos a calcular
% ProfundidadActual es el nivel en el que se encuentra éste bloque de información
% ProfundidadFija es la profunidad que solicita el usuario
% NewResultados es la lista de todos los resultados previo a seleccionar el mejor camino 
%
% Caso base: terminó de recorrer la lista
estrategia([], _ProfundidadFija, []).

% Caso recursivo: el predicado termina cuando se logró capturar todas las celdas
estrategia([R1|Rs], ProfundidadFija, [R1 | ResultadoFinal]) :-
		R1 = [_Grid, _XY, ListaCapturados, _SecuenciaColores, _ProfActual],
		length(ListaCapturados, 196),
		!, 
		estrategia(Rs,ProfundidadFija , ResultadoFinal).

% Caso recursivo: ya recorrimos toda la profundidad
estrategia([R1|Rs],ProfundidadFija, [R1 | ResultadoFinal]) :-
    R1 = [_Grid, _XY, _ListaCapturados, _SecuenciaColores,ProfundidadFija],
    !, 
	estrategia(Rs,ProfundidadFija, ResultadoFinal).

% Caso recursivo: el juego no ha terminado, ni se terminó de recorrer toda la profundidad
estrategia([R1|Rs],ProfundidadFija, NewResultados) :-
	getInformacionDeNivel(R1, InfoNivel),
    append(Rs, InfoNivel, Resultados),
    estrategia(Resultados,ProfundidadFija, NewResultados).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getInformacionDeNivel( +[Grid,[X,Y],ListaCapturados,SecuenciaColores, ProfundidadActual], -Resultado):
%
% Grid es la grilla actual
% [X,Y] es la celda origen
% ListaCapturados es la lista actual de celdas capturadas
% SecuenciaColores es la secuencia de colores que vamos a calcular
% ProfundidadActual es el nivel en el que se encuentra éste bloque de información
% Resultado es la lista de toda la información de un nivel
% 
getInformacionDeNivel([Grid,[X,Y],ListaCapturados,SecuenciaColores,ProfundidadActual],Resultado):-
    		NewProfundidad is ProfundidadActual+1,
    		getColor([X,Y],Grid,ColorActual),
    		delete([r,v,p,g,y,b],ColorActual,ListaColores), 
        	length(ListaCapturados,CantidadCapturados),  
    		
    		% Obtenemos todos los resultados del nivel guardandolo en Resultado
			findall([NewGrid,[X,Y],NewListaCapturados,NewSecuenciaColores,NewProfundidad], (
                 		member(Color,ListaColores),
     			 		flick(Grid,X,Y,Color,NewGrid,ListaCapturados,NewListaCapturados,NewCantidadCapturados),
        		 		NewCantidadCapturados > CantidadCapturados,
                        append(SecuenciaColores, [Color], NewSecuenciaColores)
        	), Resultado).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% mejorResultado( +[MejorActual|Resultados], -MejorTotal):
%
% MejorActual es el primer bloque de información de la lista, tomándolo como el mejor
% Resultados es el resto de los bloques de información que aún no comparé
% MejorTotal es el mejor bloque de información obtenido luego de comparar con todos los demás
% 
mejorResultado([MejorActual|Resultados],MejorTotal):- mejorResultadoAux([MejorActual| Resultados], MejorActual, MejorTotal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% mejorResultadoAux( +[Resultado|Resultados], +MejorActual, -MejorTotal)
%
% Resultado es el primer bloque de información de la lista tomándolo como el mejor
% Resultados es el resto de los bloques de información que aún no comparé
% MejorActual es el mejor bloque de información hasta el momento
% MejorTotal es el mejor bloque de información obtenido luego de comparar con todos los demás
% 
mejorResultadoAux([], MejorActual, MejorActual).
mejorResultadoAux([Resultado|Resultados], MejorActual, MejorTotal):-
    	getMejorSecuencia(Resultado, MejorActual, MejorLocal), 
   		mejorResultadoAux(Resultados, MejorLocal, MejorTotal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getMejorSecuencia(+Resultado1, +Resultado2, +Resultado)
%
% Resultado1 y Resultado2 son los 2 bloques de información a comparar
% Resultado es el bloque que tiene la mayor cantidad de celdas capturadas entre los otros 2
% 
getMejorSecuencia(Resultado1,Resultado2,Resultado1):-
    	Resultado1 = [_Grid1,_Origen1,ListaCapturados1,_SecuenciaColores1,_ProfInicial1],
    	Resultado2 = [_Grid2,_Origen2,ListaCapturados2,_SecuenciaColores2,_ProfInicial2],
    	length(ListaCapturados1, Tamanio1),
    	length(ListaCapturados2, Tamanio2),
    	Tamanio1 > Tamanio2,
    	!.
getMejorSecuencia(_Resultado1,Resultado2,Resultado2).    	
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%iniciarConOrigenDefault(+Grid, +Color, -NewGrid, -NewListaCapturados, -CantidadCapturados):-
%
% Grid es la grilla actual.
% Color es el color elegido por el usuario, sin que haya elegido un origen,
%       por lo tanto por defecto el origen es el 0,0.
% Si Color es el mismo que el del origen, entonces retorno Grid,
%       caso contrario retorno NewGrid
% NewListaCapturados retorna la nueva lista de celdas capturadas
% En CantidadCapturados retorna la cantidad de celdas capturadas 
iniciarConOrigenDefault(Grid,Color,Grid,NewListaCapturados,CantidadCapturados):-
        getColor([0,0],Grid,Color),
        !,
        adyCStar([0,0],Grid,NewListaCapturados),
        length(NewListaCapturados,CantidadCapturados).
iniciarConOrigenDefault(Grid,Color,NewGrid,NewListaCapturados,CantidadCapturados):-
        reemplazarEnGrilla(Grid,[0,0],Color,NewGrid),
        adyCStar([0,0],NewGrid,NewListaCapturados),
        length(NewListaCapturados,CantidadCapturados).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% iniciarConOrigenSeleccionado(+Grid, +X, +Y, -Color, -NewListaCapturados, -CantidadCapturados):- 
% 
% En Grid recibo la grilla actual
% En X,Y recibe las coordenadas de origen para el inicio del juego
% En Color retorno el color de la celda origen
% En NewListaCapturados retorno la lista de celdas capturadas 
% En CantidadCapturados retorna la cantidad de celdas capturadas 
iniciarConOrigenSeleccionado(Grid,X,Y,Color,NewListaCapturados,CantidadCapturados):-
		getColor([X,Y],Grid,Color),
		adyCStar([X,Y],Grid,NewListaCapturados),
		length(NewListaCapturados,CantidadCapturados).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +X, +Y, +Color, -NewGrid,-CantidadCapturados)
% 
% NewGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla.
% En CantidadCapturados retorna la cantidad de celdas capturadas 
% En X,Y recibe las coordenadas de origen para el inicio del juego
flick(Grid,X,Y,Color,NewGrid,ListaCapturados,NewListaCapturados,CantidadCapturados):-

			% Obtengo el color de la celda origen, y si es igual retorno falso
    		getColor([X,Y],Grid,C),
        	C \= Color,

			% Pinto las celdas que ya tenia capturadas
    		pintarCapturados(Grid,ListaCapturados,Color,NewGrid),

			% Obtengo la nueva lista de celdas capturadas
    		adyCStar([X,Y],NewGrid,NewListaCapturados),

			% Calculo la cantidad de celdas capturadas
    		length(NewListaCapturados,CantidadCapturados).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INICIO ADYACENTES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * adyCStar(Origin, +Grid, -Res)
 * Calcula el conjunto de celdas adyacentesC* de la celda Origin en la grilla Grid
 * siguiendo una estrategia de propagación o expansión.
 */

adyCStar(Origin, Grid, Res) :-
    adyCStarSpread([Origin], [], Grid, Res).

/*
 * adyCStarSpread(+Pend, +Vis, +Grid, -Res)
 * Pend: por "pendientes", inicialmente es la lista [Origin], y en general es 
 * el conjunto de celdas adyacentesC* a Origin que aún no fueron consideradas.
 * Vis: por "visitados", inicialmente [], son las celdas adyacentesC* a la Origen 
 * que ya fueron consideradas.
 * Grid: idem adyCStar
 * Res: idem adyCStar
 * En cada paso se selecciona una celda de las pendientes, se pasa a visitados, y
 * se agregan a pendientes todas aquellas adyacentes a la celda, del mismo color, que no estén
 * ya ni en pendientes ni visitados.
 */

adyCStarSpread([], Vis, _Grid, Vis).

adyCStarSpread(Pend, Vis, Grid, Res):-
    Pend = [P|Ps],
    findall(A, 
	        (
    	        adyC(P, Grid, A),
        	    not(member(A, Pend)),
            	not(member(A, Vis))
	        ), 
            AdyCP),
    append(AdyCP, Ps, NPend),
    adyCStarSpread(NPend, [P|Vis], Grid, Res).

/* 
 * adyC(+P, +Grid, -A)
 */

adyC(P, Grid, A):-
    ady(P, Grid, A),
    getColor(P, Grid, C),
    getColor(A, Grid, C).

/* 
 * ady(+P, +Grid, -A)
 */

ady([X, Y], Grid, [X1, Y]):-
    length(Grid, L),
    X < L - 1,
    X1 is X + 1.

ady([X, Y], _Grid, [X1, Y]):-
    X > 0,
    X1 is X - 1.

ady([X, Y], Grid, [X, Y1]):-
    Grid = [F|_],
    length(F, L),
    Y < L - 1,
    Y1 is Y + 1.

ady([X, Y], _Grid, [X, Y1]):-
    Y > 0,
    Y1 is Y - 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% pintarCapturados(+Grid, +[], +Color,-Grid)
% pintarCapturados(+Grid, +[Celda|ListaCeldas], +Color, -NewGrid)
%
% Dada una grilla Grid, una lista de celdas capturadas, y un color
% Pinta todas las celdas capturadas y retorna la nueva grilla NewGrid
pintarCapturados(Grid,[],_Color,Grid). 
pintarCapturados(Grid,[Celda|ListaCeldas],Color, NewGrid):-
			reemplazarEnGrilla(Grid,Celda,Color,NewGrid1),
			pintarCapturados(NewGrid1,ListaCeldas,Color,NewGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% reemplazarEnGrilla(+Grid, +[X|[Y]], +NewElement, -NewGrid)
% 
% Dada una grilla Grid y una posicion (X,Y)
% Busca el elemento y lo reemplaza por NewElement
% Retorna la grilla NewGrid con el elemento reemplazado
reemplazarEnGrilla(Grid,[X|[Y]],NewElement,NewGrid):- 
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
% getColor(+Grid,+X,+Y,-C)
% 
% Dada una grilla Grid, retorna el color C de una posicion (X,Y)
getColor([X,Y], Grid, C):-
    nth0(X, Grid, F),
    nth0(Y, F, C). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getRecords(-RecordsOrdenados)
% 
% Obtengo una tabla con todos los records
getRecords(RecordsOrdenados):-
    	% busco todos los ganadores
    	findall([Nick,Turnos],
                		ganador(Nick,Turnos),
      			RecordsDesordenados),
    	% ordeno los ganadores segun sus turnos (de menor a mayor)
		sort(2, @=<, RecordsDesordenados, RecordsOrdenados).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% newRecord(+Nick, +NewTurnos, -NewRecords)
% 
% Dado un Nombre y una Cantidad de turnos,
% Se lo registra en la base de datos, y se retorna la nueva lista de records
%
% Caso 1: el usuario no existe en la base de datos, por lo tanto registramos su puntuacion
newRecord(Nick,Turnos,NewRecords):-      		
           not(ganador(Nick,_)),           
           assert(ganador(Nick,Turnos)),
           getRecords(NewRecords),
           !.
% Caso 2: el usuario ya existe en la base de datos
newRecord(Nick,Turnos,NewRecords):-  
      % buscamos su puntuacion
      ganador(Nick,OldTurnos),
      % solo registramos su puntuacion si hizo un record
      Turnos < OldTurnos,
	  !,
      retract(ganador(Nick,OldTurnos)),
      assert(ganador(Nick,Turnos)),    
   	  getRecords(NewRecords).
% Caso 3: siempre retorna verdadero
newRecord(_,_,Records):- getRecords(Records).

