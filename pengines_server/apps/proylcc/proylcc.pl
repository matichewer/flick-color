:- module(proylcc, 
	[  
		flick/8
	]).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getGrillasDeUnNivel( +Grid, +[X,Y], +ColorActual, +ListaCapturados,
% 					+ SecuenciaColores, -NewSecuenciaColores, ListaDeGrillas):-
% 
% El parámetro ListaCapturados es mejor recibirlo por parámetro CREO.
% Lo podemos calcular acá mismo, pero sería al pedo otra vez pedirlo. Sería mejor recibirlo por parámetro
% 
% CONSULTA DE EJEMPLO
% 	init3(Grid), getInformacionDeNivel([Grid,[0,0],[[0,0]],[y]],Resultado).
getInformacionDeNivel([Grid,[X,Y],ListaCapturados,SecuenciaColores,Profundidad],Resultado):-
    		    		
    		% recalculamos la profundidad restante
    		NewProfundidad is Profundidad+1,
    
    		% Obtenemos el color actual
    		getColor([X,Y],Grid,ColorActual),
    
    		% Borramos el color actual de la lista de colores
    		delete([r,v,p,g,y,b],ColorActual,ListaColores), 
    
    		% Calculamos el tamaño de la lista de celdas capturadas
        	length(ListaCapturados,CantidadCapturados),  
    		
    		% Obtenemos todos los resultados del nivel
			findall([NewGrid,[X,Y],NewListaCapturados,NewSecuenciaColores,NewProfundidad], (
                 		member(Color,ListaColores),
     			 		flick(Grid,X,Y,Color,NewGrid,ListaCapturados,NewListaCapturados,NewCantidadCapturados),
        		 		NewCantidadCapturados > CantidadCapturados,
                        append(SecuenciaColores, [Color], NewSecuenciaColores)
        		), Resultado).


% formato de la info q movemos: [Grid,Origen,ListaCapturados,SecuenciaColores,Profundidad]

/* CONSULTA:
init3(Grid), 
estrategia([[Grid,[0,0], [[0,0]], [],0]], 1,Resultado).
*/

% No hay que recorrer mas nada
estrategia([], _Profundidad, []).

% Ya es una partida ganada
estrategia([Resultado|Resultados], Profundidad, [Resultado | ResultadoFinal]) :-
    Resultado = [_Grid, _XY, ListaCapturados, _SecuenciaColores,_Prof],
    length(ListaCapturados, 196),
    !, 
	estrategia(Resultados,Profundidad , ResultadoFinal).

% Ya recorrimos toda la profundidad
estrategia([Resultado | Resultados],Profundidad, [Resultado | ResultadoFinal]) :-
    Resultado = [_Grid, _XY, _ListaCapturados, _SecuenciaColores,Profundidad],
    !, 
	estrategia(Resultados,Profundidad, ResultadoFinal).

% El juego no ha terminado, ni se terminó de recorrer en profundidad
estrategia([Resultado|Resultados],Profundidad, NewTodosLosResultados) :-
	getInformacionDeNivel(Resultado, NewResultado),
    append(Resultados, NewResultado, TodosLosResultados),
    estrategia(TodosLosResultados,Profundidad, NewTodosLosResultados).



/* CONSULTA:

Profundidad = 2,
init3(Grid),
X=0, Y=0,
adyCStar([X,Y],Grid,ListaCapturados),
botonAyuda(Grid, [0,0], ListaCapturados,Profundidad, SecuenciaColores, CantidadAdyacentes). 

*/

% botonAyuda( +Grid, +Origen, +ListaCapturados, +Profundidad, -Secuencia, -CantidadAdyacentes):-
botonAyuda(Grid, Origen, ListaCapturados,Profundidad, SecuenciaColores, CantidadAdyacentes):-
    	
    	% [Grid,Origen,ListaCapturados,SecuenciaColores,ProfInicial]
    estrategia([ [Grid,Origen,ListaCapturados,[],0] ], Profundidad, TodosLosResultados),
    
    mejorResultado(TodosLosResultados, R),
    R = [_Grid,_Origen,NewListaCapturados,SecuenciaColores, _ProfInicial],
    length(NewListaCapturados,CantidadAdyacentes).
    

% Asumimos que el primer resultado es el mejor
mejorResultado([MejorActual|Resultados],MejorTotal):- mejorResultadoAux([MejorActual| Resultados], MejorActual, MejorTotal).

% Comparamos con todos los resultados
mejorResultadoAux([], MejorActual, MejorActual).

mejorResultadoAux([Resultado|Resultados], MejorActual, MejorTotal):-
    	getMejorSecuencia(Resultado, MejorActual, MejorLocal), 
   		mejorResultadoAux(Resultados,MejorLocal, MejorTotal).

% Comparamos la cantidad de celdas capturadas de 2 resultados
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

		% Obtengo el color de la celda origen y la retorno para agregarla al historial
		getColor([X,Y],Grid,Color),

		% Obtengo la nueva lista de celdas capturadas
		adyCStar([X,Y],Grid,NewListaCapturados),

		% Calculo la cantidad de celdas capturadas
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


%%%%%%%%%%%%%%%%%% CODIGO SUPER EFICIENTE %%%%%%%%%%%%%%%%%%%%%%

/*
 * adyCStar(Origin, +Grid, -Res)
 * Calcula el conjunto de celdas adyacentesC* de la celda Origin en la grilla Grid
 * siguiendo una estrategia de propa	gación o expansión.
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

%%%%%%%%%%%%%% FIN CODIGO SUPER EFICIENTE %%%%%%%%%%%%%%%






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
