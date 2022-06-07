:- module(proylcc, 
	[  
		flick/8
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getGrillasDeUnNivel( +Grid, +X, +Y, +ColorActual, +ListaCapturados,+ SecuenciaColores, -NewSecuenciaColores, ListaDeGrillas):-
% 
% El parámetro ListaCapturados es mejor recibirlo por parámetro CREO.
% Lo podemos calcular acá mismo, pero sería al pedo otra vez pedirlo. Sería mejor recibirlo por parámetro
% CONSULTA DE EJEMPLO: init3(Grid), getGrillasDeUnNivel(Grid,0,0,y,[[0,0]],NewListaCapturados,[y], NewSecuenciaColores, ListaDeGrillas)
getGrillasDeUnNivel(Grid,X,Y,ColorActual,ListaCapturados, NewListaCapturados,SecuenciaColores,NewSecuenciaColores,ListaDeGrillas):-
              
    		delete([r,v,p,g,y,b],ColorActual,ListaColores),           
			findall(NewGrid, (
                 		member(C,ListaColores),
     			 		flick(Grid,X,Y,C,NewGrid,ListaCapturados,NewListaCapturados,NewCantidadCapturados),
        				length(ListaCapturados,CantidadCapturados),
        		 		NewCantidadCapturados > CantidadCapturados,
                        append(SecuenciaColores, C, NewSecuenciaColores)
        		), ListaDeGrillas).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% estrategia(X,Y+Profundidad, -ListaColores, -NewCapturadas)
%
%estrategia(X,Y,Grid,Profundidad,Lista,ColoresMejorJugada,NewCapturadas):-
%    	getColor([X,Y],Grid,Color),   
%    	delete([r,p,g,b,y,v],Color, ListaColoresArbol),    	
%    	recorrerColores([X,Y],Grid).                      
    
    	
%recorrerColores([Color|ListaColores], ):-
%    	flick(Grid,X,Y,Color,NewGrid,ListaCapturados,NewListaCapturados,CantidadCapturados),
    	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% cincoColores(+Color,+Lista, -ListaColores)
%
%cincoColores(Color,Lista, ListaColores):-
%    	delete(Lista,Color,ListaColores).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% inicializar(+Grid, +X, +Y, -Color, -NewListaCapturados, -CantidadCapturados):- 
% 
% En X,Y recibe las coordenadas de origen para el inicio del juego
% En NewListaCapturados retorno la lista de celdas capturadas 
% En CantidadCapturados retorna la cantidad de celdas capturadas 

inicializar(Grid,X,Y,Color,NewListaCapturados,CantidadCapturados):-

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
