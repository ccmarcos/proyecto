oracion-->gn,gv.
gn-->pro,verbo,condiCortesia,prepo,det,verbo,otros,det,sust,proInva,verbo,otros,prepo,sust,condiCortesia,prepo,sust,elemento.
gv-->verbo.
det-->[la];[un];[los];[el];[].
verbo-->[averiguar];[buscar];[podras];[mostrarme];[hablen];[puedes].
verbo-->[deseo];[mostrar];[ayudame];[desplegar];[ayudame];[decirme].
verbo-->[hay];[quiero];[este];[listar];[visualizar];[encontrar].
verbo-->[trate];[hable];[ayudar];[ensenar];[sugerirme];[averiguar];[presentarme];[].
condiCortesia-->[encarecidamente];[por_favor];[].
sust-->[libros];[informacion];[autor];[ejemplares].
sust-->[libro];[temas];[anio];[ficha];[tema].
sust-->[clasificacion];[titulo];[area];[].
proInva-->[que];[].
pro-->[me].
pro-->[].
otros-->[relacionados];[relacionado];[toda];[todos];[relacionada];[cuantos];[sobre];[algunos];[].
prepo-->[con];[a];[de];[del];[el];[].
elemento-->[_Palabra].

inicio1:-
	[library(rdf)],
	use_module(library(semweb/rdf_db)),
	expand_file_name('SO/*.rdf',A),
	rdf_load([A],[format(xml)]).		  % carga la informacion de todos los libros

inicio:-
	[library(rdf)],
	use_module(library(semweb/rdf_db)),
	expand_file_name('SO/*.rdf',A),		   % carga la informacion de todos los libros
	expand_file_name('Descripcion_SO.rdf',E),  % carga la informacion de tipo
	rdf_load([A,E],[format(xml)]),
	write('Ingresar consulta:'),put(32),       % pedir consulta al usuario
	read(Frase),                               % lee la consulta escrita por el usuario
	atomic_list_concat(L,',',Frase),	   % separa la frase por comas y la guarda en L
	oracion(L,F),                              % se encargan de verificar la oracion
	verifica(F,L).				   % se encargan de verificar la oracion

% obtiene un  en la posicion X
posicion([Y|_],1,Y).
posicion([_|Rs],N,D):-N2 is N-1,
	posicion(Rs,N2,D).

% devuelve numero de elementos
longitud([],0).
longitud([_X|R],Long):-longitud(R,Aux),Long is 1+Aux.

% verifica que la frase sea correcta
verifica([_X],_Lista):-write('Tu frase esta incorrecta, vuelve a intentarlo').

verifica([],Lista):-longitud(Lista,NL),          % NL guarda la longitud de la frase (L)
	posicion(Lista,NL,Buscar),               % la ultima frase de la lista L es lo que se va a Buscar (NL), el resultado lo devuelve en: Buscar
           M is NL-1,posicion(Lista,M,Tipo),     % la penultima frase de la lista L es el Tipo de busqueda (NL-1), resultado devuelto en: Tipo
	consultas(Tipo,Buscar).			 % consulta (NL-1, NL), el Tipo de busqueda y lo que se desea buscar

% Consulta por Tipo: ficha
consultas('ficha', ABuscar):-
	 ficha(ABuscar,_,_,_,_,_,_).

% Consulta por Tipo: titulo
consultas('titulo',ABuscar):-
	titulo(_,ABuscar,_,_,_,_,_).

% Consulta por Tipo: autor
consultas('autor',ABuscar):-
	autor(_,_,ABuscar,_,_,_,_).

% Consulta por Tipo: clasificacion
consultas('clasificacion',ABuscar):-
	clasificacion(_,_,_,ABuscar,_,_,_).

% Consulta por tipo: anio
consultas('anio',Abuscar):-
	anio(_,_,_,_,Abuscar,_,_).

% Consulta por tipo: ejemplares
consultas('ejemplares',ABuscar):-
	ejemplares(_,_,_,_,_,ABuscar,_).

% Consulta por tipo: area
consultas('area',ABuscar):-
	area(_,_,_,_,_,_,ABuscar).

% Consulta por tipo: tema
consultas('tema',ABuscar):-
       tema(ABuscar,_,_,_,_,_,_,_).

info(Archivo, Ficha, Titulo, Autor, Clasif, Anio, Ejem, Area):-
	rdf(Archivo, 'Biblioteca/ficha', Ficha),
	rdf(Archivo, 'Biblioteca/titulo', Titulo),
	rdf(Archivo, 'Biblioteca/autor',  Autor),
	rdf(Archivo, 'Biblioteca/clasificacion', Clasif),
	rdf(Archivo, 'Biblioteca/anio', Anio),
	rdf(Archivo, 'Biblioteca/ejemplares', Ejem),
	rdf(Archivo, 'Biblioteca/area', Area).

% Buscar por ficha
ficha(Ficha, Titulo, Autor, Clasif, Anio, Ejem, Area):-
	rdf(X, 'Biblioteca/ficha', literal(Ficha)),
           info(X, literal(Ficha), Titulo, Autor, Clasif, Anio, Ejem, Area),
	write('Titulo:'),write(Titulo),nl,write(Autor),nl,write(Clasif),nl,write(Anio),nl,write(Ejem),nl,write(Area),nl.

% Buscar por titulo
titulo(Ficha, Titulo, Autor, Clasif, Anio, Ejem, Area):-
	rdf(X, 'Biblioteca/titulo', literal(Titulo)),
	info(X, Ficha, literal(Titulo), Autor, Clasif, Anio, Ejem, Area),
	write(Ficha),nl,write(Autor),nl,write(Clasif),nl,write(Anio),nl,write(Ejem),nl,write(Area),nl.


% Buscar por autor
autor(Ficha, Titulo, Autor, Clasif, Anio, Ejem, Area):-
	rdf(X, 'Biblioteca/autor', literal(Autor)),
	info(X, Ficha, Titulo, literal(Autor), Clasif, Anio, Ejem, Area),
	write(Ficha),nl,write(Titulo),nl,write(Clasif),nl,write(Anio),nl,write(Ejem),nl,write(Area),nl.


% Buscar por clasificacion
clasificacion(Ficha, Titulo, Autor, Clasif, Anio, Ejem, Area):-
	rdf(X, 'Biblioteca/clasificacion', literal(Clasif)),
	info(X, Ficha, Titulo, Autor, literal(Clasif), Anio, Ejem, Area),
	write(Ficha),nl,write(Titulo),nl,write(Autor),nl,write(Anio),nl,write(Ejem),nl,write(Area),nl.


% Buscar por anio
anio(Ficha, Titulo, Autor, Clasif, Anio, Ejem, Area):-
	rdf(X, 'Biblioteca/anio', literal(Anio)),
	info(X, Ficha, Titulo, Autor, Clasif, literal(Anio), Ejem, Area),
	write(Ficha),nl,write(Titulo),nl,write(Autor),nl,write(Clasif),nl,write(Ejem),nl,write(Area),nl.


% Buscar por numero de ejemplares
ejemplares(Ficha, Titulo, Autor, Clasif, Anio, Ejem, Area):-
	rdf(X, 'Biblioteca/ejemplares', literal(Ejem)),
	info(X, Ficha, Titulo, Autor, Clasif, Anio, literal(Ejem), Area),
	write(Ficha),nl,write(Titulo),nl,write(Autor),nl,write(Clasif),nl,write(Anio),nl,write(Area),nl.

% Buscar por area
area(Ficha, Titulo, Autor, Clasif, Anio, Ejem, Area):-
	rdf(X, 'Biblioteca/area', literal(Area)),
	info(X, Ficha, Titulo, Autor, Clasif, Anio, Ejem,literal(Area)),
	write(Ficha),nl,write(Titulo),nl,write(Autor),nl,write(Clasif),nl,write(Anio),nl,write(Ejem),nl.


tema(Tema,Ficha,Titulo,Autor,Clasif,Anio,Ejemplares,Area):-
	rdf(_, 'Biblioteca/tema', literal(Tema),W) ,
	compound_name_arguments(W,_,[L|_]),
	info(L,Ficha,Titulo,Autor,Clasif,Anio,Ejemplares,Area).

%subtema(Tema,Ficha,Titulo,Autor,Clasif,Anio,Ejemplares,Area):-
%	rdf(_, 'Biblioteca/subtema', literal(Tema),W) ,
%	compound_name_arguments(W,_,[L|_]),
%	info(L,Ficha,Titulo,Autor,Clasif,Anio,Ejemplares,Area).





















