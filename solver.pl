% Autor: Kateřina Zákravská
% LS 2014/2015

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Nonogramy %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Nastavení %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
allow_backtrack :- false. % zakazuje tipování
%allow_backtrack :- true. % povoluje tipování

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%  I/O  %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% hlášky
io_spatny :- nl, write('Vstupní soubor má špatný formát.'), nl.

io_uvod :-
		write('Vítejte v řešiči malovaných křížovek (nonogramů)'), nl,
		write('Pro spuštění programu napište \"nono.\"'), nl.

io_vstupni_soubor :- nl, write('Zadejte cestu k vstupnímu souboru (i s příponou, pokud nějakou má) ve formátu: \n\t\t\'cesta_k_souboru.pripona\'.'), nl.


io_vystupni_soubor :-
		nl, write('Zadejte název výstupního souboru ve formátu: \'název_souboru\'.'), nl,
		write('Pokud jej nezadáte, použije se \'default.out\' jako název.'), nl.

io_neuplne :- nl, write('Křížovku nelze jednoznačně vyřešit. Chcete povolit backtrackování? [ano / NE]'), nl.

% načtení zadání pro křížovku
% io_nactiVstup(+In, -Height, -Width, -ListLines, -ListCols) :- In je vstupní
% soubor, vrací se seznam seznamů ListLines pro řádky, ListCols pro sloupce,
% počet řádků Height, počet sloupců Width.
io_nacti_vstup(In, Height, Width, ListLines, ListCols) :- 
		see(In),						% otevření souboru
		read(Height),					% přečtení výšky křížovky
		read(Width),					% přečtení šířky křížovky
		io_zpracuj(Height, ListLines),	% načtení zadání pro řádky
		io_zpracuj(Width, ListCols),	% načtení zadání pro sloupce
		seen.							% uravření souboru

% io_zpracuj(+N, -List) :- Přečte N řádků a uloží do seznamu List.
io_zpracuj(0, []).
io_zpracuj(N, [Line |Vysl]) :-
		N > 0, N1 is N-1,
		read(Line),
		(Line == end_of_file, !, io_spatny, fail ; true), % soubor je krátký
		io_zpracuj(N1, Vysl).

% io_matice(+Matice) :- vypíše křížovku na stdout.
io_vypis([[]]) :- nl.
io_vypis( [ [] | Mat]) :- nl, io_vypis(Mat).
io_vypis([ [Hr | Radek]  | Mat ]) :-
		(
			Hr =@= 'b', write('.') ;
			Hr =@= 'c', write('#') ;
			var(Hr), write('?')
		),
		write(' '), io_vypis([Radek | Mat]).

% io_vystup_soubor(+Matice, +Out) :- vypíše Matici do souboru Out.
io_vystup_soubor(Matice, Out) :-
		tell(Out),
		io_vypis(Matice),
		told.

% read_an(-B) :- přečte odpověď uživatele a vrátí ji jako true, pokud je
% odpovědí ano, false v případě ne. Defaultní odpověď se pozná podle přečteného
% prázdného řetězce.
read_an(B) :-
	current_input(I),
	read_line_to_codes(I, X),
	(
		X == [], B = true;
		(
			X == "a", B = true, ! ;
			X == "A", B = true, ! ;
			X == "n", B = false, ! ;
			X == "N", B = false
		)
	).

% io_load(-Height, -Width, -ListLines, -ListCols) :- postará se o načtení
% vstupu.
io_load(Height, Width, ListLines, ListCols) :-
	write('Mám načíst vstup ze souboru? (A/n)'), nl,
		read_an(B),
		(
			B == true, io_vstupni_soubor, read(Input),
			io_nacti_vstup(Input, Height, Width, ListLines, ListCols) ;
			io_vstup_stdin(Height, Width, ListLines, ListCols)
		).

% io_vstup_stdin(-Height, -Width, -ListLines, -ListCols) :- Přečte zadání
% křížovky z terminálu a zjištěné hodnoty vrátí.
io_vstup_stdin(Height, Width, ListLines, ListCols) :-
	write('Zadejte počet řádků křížovky (ukončete tečkou).'), nl,
		read(Height),
	nl, write('Zadejte počet sloupečků křížovky (ukončete tečkou).'), nl,
		read(Width),
	nl, write('Zadejte zadání pro řádky křížovky (na samostatné řádky). Čísla oddělujte čárkou, řádek začněte symbnolem "[" a ukončete symboly "]."'), nl,
		io_zpracuj(Height, ListLines),
	nl, write('Zadejte zadání pro sloupečky křížovky (na samostatné řádky). Čísla oddělujte čárkou, řádek začněte symbnolem "[" a ukončete symboly "]."'), nl,
		io_zpracuj(Width, ListCols).

% io_save(+Matice) :- dotáže se na výpis a uložení řešení, popř. volá výpis a ukládání.
io_save(Matice) :-
	nl, write('Chcete řešení vypsat na obrazovku (do terminálu)? (A/n)'), nl,
		read_an(B),
			(
				B == true, io_vypis(Matice), ! ;
				true
			),
	nl, write('Mám řešení zapsat do souboru? (A/n)'), nl,
		read_an(B2),
			(
				(
					B2 == true, io_vystupni_soubor,
					current_input(I), read_line_to_codes(I, Out),
					(
						Out == [], io_vystup_soubor(Matice, 'default.out') ;
						preved_na_text(Out, Out2), % převede na seznam charů
						atomic_list_concat(Out2, Out3), % převede na 1 řetězec
						io_vystup_soubor(Matice, Out3) % zapíše do souboru
					)
				), ! ;
				true
			).

% io_kontrola_var(+Matice) :- zkontroluje, jestli je v Matici nějaká volná
% proměnná a pokud ano, informuje o tipování.
io_kontrola_var(Matice) :-
	(
		var_mat(Matice),
		nl, write('Tuto křížovku neumím vyřešit bez tipování. Povolit ho lze ve zdrojovém kódu pomocí:'), nl,
		write('\t\t\t allow_backtrack :- true.'), nl,
%		write('Chcete povolit tipování a křížovku dořešit?'), nl,
%			read_an(B),
%			(
%				B == true, ! ;
%				write('Vypisuji neúplné řešení.'), nl
		nl, write('Vypisuji neúplné řešení.'), nl
	) ;
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%  Reprezentace křížovky  %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% prazdna_matice(R, S, Matice) :- vytvoří prázdnou matici rozměrů R x S, plnou
% volných proměnných.
prazdna_matice(0, _, []).
prazdna_matice(R, S, [Radek | Matice]) :-	prazdny_radek(S, Radek), R1 is R-1,
											prazdna_matice(R1, S, Matice).

% prazdny_radek(N, Radek) :- vytvoří prázdný řádek matice o délce N.
prazdny_radek(0, []).
prazdny_radek(N, [ _ | Radek]) :- N1 is N - 1, prazdny_radek(N1, Radek).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%  Pomocné predikáty  %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% transp(+M, -TM) :- TM je transpozicí matice M
transp([ [] |_], []).
transp(M, [Col|TMat]) :- column(M, Col, ZMat), transp(ZMat, TMat).

% sloupec(+M, -S, -Z) :- S je seznam prvků prvního sloupce matice M, Z je
% matice zbylých prvků.
column([], [], []).
column([[First|ZLine] | ZMat], [First|ZCol], [ZLine|ZMat2]) :- column(ZMat, ZCol, ZMat2).

% var_mat(+Matice) :- otestuje, zda se v matici vyskytuje alespoň jedna volná
% proměnná.
var_mat( [[] | Mat ]) :- var_mat(Mat).
var_mat([ [H | Radek] | Mat]) :- var(H), ! ; var_mat([Radek | Mat]).

% preved_na_text(+List, -Vysl) :- Seznam ASCII hodnot List převede na seznam
% charů Vysl.
preved_na_text([], []).
preved_na_text([X | Zbytek], [A | Vysl]) :- char_code(A, X), preved_na_text(Zbytek, Vysl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%  Solve nonogram  %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% lmost(+Zadani, +Radek, -VyslRadek) :- Pro dané Zadani a Radek najdne
% nejlevější možné umístění černých bloků. Vrací jako seznam VyslRadek.
lmost([], [], []). % umístění posledního bloku z řádku

% Všechny bloky už byly umístěny, zbytek řádku by tedy měl být bílý. Problém,
% pokud zbylo nějaké černé políčko.
lmost([], [Hr | Radek], [Hv | Vysl]) :-	Hr =@= 'c', !, fail;
										Hv = 'b', lmost([], Radek, Vysl).

lmost([Hz | Zad], [Hr | Radek], [ Hv | Vysl]) :-
		Hr =@= 'b', !, Hv = 'b', lmost([Hz | Zad], Radek, Vysl);					% políčko je bílé
		Hr =@= 'c', !, Hz1 is Hz - 1, Hv = 'c', lmost_c([Hz1 | Zad], Radek, Vysl);	% políčko je černé, vynucení souvislosti
		Hz1 is Hz - 1, Hv = 'c', lmost_c([Hz1 | Zad], Radek, Vysl);					% volné políčko, umístí se černá
		Hv = 'b', lmost([Hz | Zad], Radek, Vysl).									% nepovedlo se umístit černou, bude tedy bílé

% lmost_c(+Zadani, +Radek, -Vysl) :- pro dané Zadani a Radek najde nejlevější
% umístění černých bloků. Vynucuje, že začíná umisťovat černý blok (nikoliv
% bílý), souvisle.

% Pokud ještě není umístěn celý blok a aktuální políčko není bílé, umístí
% jeden černý dílek a rekurzivně umisťuje zbytek bloku (souvisle).

% Pokud je umístěn celý blok, je potřeba za ním vynutit bílé políčko (pokud už
% nebylo černé).
lmost_c([0], [], []). % umístění posledního bloku z řádku
lmost_c([Hz | Zad], [Hr | Radek], [ Hv | Vysl]) :-
		Hz > 0, !, Hr \=@= 'b', Hz1 is Hz - 1, Hv = 'c', lmost_c([Hz1 | Zad], Radek, Vysl);
		Hr \=@= 'c', !, Hv = 'b', lmost(Zad, Radek, Vysl).

% prunik2(+Lmost, +Rmost, +PuvRadek, -Zmeneno) :- najde průnik
% nelevějšího Lmost a nejpravějšího Rmost uspořádání. Zmeneno bude 1, pokud se
% určilo alespoň jedno políčko oproti PuvRadek.
prunik(Lmost, Rmost, PuvRadek, Zmeneno) :- prunik2(Lmost, 0, 'b', Rmost, 0, 'b', PuvRadek, Zmeneno), !.

% prunik2(+Lmost, +L, +Lb, +Rmost, +R, +Rb, +PuvRadek, -Zmeneno) :- Jako
% prunik/3, ale je potřeba hledat průnik stejných bloků, kde čísla černých
% bloků jsou L a R. Proměnné Lb a Rb signalizují, zda byl předchozí blok bílý
% nebo černý (hodnoty 'b' a 'c').
prunik2([], _, _, [], _, _, [], _) :- !.
prunik2([Hl | Lmost], L, Lb, [Hr | Rmost], R, Rb, [Hp | PuvRadek], Zmeneno) :-
% předchozí políčko bylo bílé, aktuální je černé --> číslo černého bloku +1
		(Lb =@= 'b', Hl =@= 'c', L1 is L + 1 ; L1 is L),
		(Rb =@= 'b', Hr =@= 'c', R1 is R + 1 ; R1 is R),
		(L1 =@= R1, Hl =@= Hr, % políčka jsou stejná
			(Hl \=@= Hp, Hp = Hl, Zmeneno = 1 ; true) ; % hodnota nebyla v původním řádku --> Zmeneno = 1
		true),
		prunik2(Lmost, L1, Hl, Rmost, R1, Hr, PuvRadek, Zmeneno), !.

% tipnout(+Matice) :- najde první neurčené políčko a zkusí ho označit nejprve
% za černé, v případě selhání za bílé.
tipnout( [[] | Mat ]) :- tipnout(Mat).
tipnout([ [H | Radek] | Mat]) :-
		var(H), !, (H = 'c' ; H = 'b') ;	% tiplo si
		tipnout([Radek | Mat]).				% hledá další políčko

% solve_mat(+Matice, +Zadani, -Zmeneno) :- najde další políčka, která lze určit.
solve_mat([], _, _) :- !.
solve_mat([Hm | Matice], [Hz | Zadani], Zmeneno) :-
		lmost(Hz, Hm, VyslLmost), !, % nejlevější uspořádání
		reverse(Hm, Hmr), reverse(Hz, Hzr), lmost(Hzr, Hmr, VyslRm), !, reverse(VyslRm, VyslRmost), % nejpravější uspořádání
		prunik(VyslLmost, VyslRmost, Hm, Zmeneno), % průnik obou uspořádání
		solve_mat(Matice, Zadani, Zmeneno). % řešení zbytku matice

% solve(+MatRadky, +ListLines, +MatSloupce, +ListCols) :- dokud byla nějaká
% změna, znovu volá vyřešení křížovky. Pokud neproběhla změna, je možné tipovat
% (pokud je povolen bactracking).
solve(MatRadky, ListLines, MatSloupce, ListCols) :-
		solve_mat(MatRadky, ListLines, ZmenenoR),	% vyřeší řádky
		solve_mat(MatSloupce, ListCols, ZmenenoS),	% vyřeší sloupce
% pokud bylo alespoň jedno políčko změněno, zavolá rekurzivně solve/4
% jinak pokud je povolen backtrack, tak si tipně a opět volá solve/4
% pokud ani jedna z možností neproběhne, je konec
		(
			(ZmenenoR == 1 ; ZmenenoS == 1), !, solve(MatRadky, ListLines, MatSloupce, ListCols) ;
			var_mat(MatRadky), allow_backtrack, !, tipnout(MatRadky), solve(MatRadky, ListLines, MatSloupce, ListCols) ;
			true
		).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%  Main  %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% začne řešit nonogram
nono :-
	io_load(Height, Width, ListLines, ListCols),		% načtení vstupu
	prazdna_matice(Height, Width, MatRadky),			% vytvoření matice pro křížovku
	transp(MatRadky, MatSloupce), !,					% transponovaná matice
	solve(MatRadky, ListLines, MatSloupce, ListCols),	% řešení křížovky
	io_kontrola_var(MatRadky),							% kontrola neúplného řešení
	io_save(MatRadky).									% výpis a uložení řešení


% začátek programu
:-	use_module(library(readutil)),
	io_uvod.
