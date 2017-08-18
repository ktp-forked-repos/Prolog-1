:- use_module(library(clpfd)).

% per solo 3 casi non vale la pena di ricorrere alla ricorsione.
esplodi([A],A).
esplodi([A,B],10 * A + B).
esplodi([A,B,C],100 * A + 10 * B + C).

calcolo_enigmatico(Nu1,Op1,Nu2,=,Nu3, % unico scopo degli '=' è la leggibilità
                   Op4,    Op5,  Op6,
                   Nu4,Op2,Nu5,=,Nu6,
                     =,      =,    =,
                   Nu7,Op3,Nu8,=,Nu9) :-
	% raggruppa tutte le cifre usate
	append([Nu1,Nu2,Nu3,Nu4,Nu5,Nu6,Nu7,Nu8,Nu9],Tmp),
	% le ordina ed elimina i duplicati
	sort(Tmp,Cifre),
	% il possibile valore di ogni cifra
	Cifre ins 0..9,
	% tutte le cifre devono essere diverse tra di loro
	all_different(Cifre),
	% crea i 9 numeri
	esplodi(Nu1,Numero1),
	esplodi(Nu2,Numero2),
	esplodi(Nu3,Numero3),
	esplodi(Nu4,Numero4),
	esplodi(Nu5,Numero5),
	esplodi(Nu6,Numero6),
	esplodi(Nu7,Numero7),
	esplodi(Nu8,Numero8),
	esplodi(Nu9,Numero9),
	% crea le 6 operazioni
	Operazione1 =.. [Op1,Numero1,Numero2],
	Operazione2 =.. [Op2,Numero4,Numero5],
	Operazione3 =.. [Op3,Numero7,Numero8],
	Operazione4 =.. [Op4,Numero1,Numero4],
	Operazione5 =.. [Op5,Numero2,Numero5],
	Operazione6 =.. [Op6,Numero3,Numero6],
	% le 6 equazioni che devono essere soddisfatte
	Operazione1 #= Numero3,
	Operazione2 #= Numero6,
	Operazione3 #= Numero9,
	Operazione4 #= Numero7,
	Operazione5 #= Numero8,
	Operazione6 #= Numero9,
	% inizia la ricerca
	labeling([ff],Cifre),
	% stampa la soluzione
	format('~n~t~d~3| ~w~t~d~9| =~t~d~15|~n',[Numero1,Op1,Numero2,Numero3]),
	format('~t~w~3|~t~w~9|~t~w~15|~n',[Op4,Op5,Op6]),
	format('~t~d~3| ~w~t~d~9| =~t~d~15|~n',[Numero4,Op2,Numero5,Numero6]),
	format('~t=~3|~t=~9|~t=~15|~n'),
	format('~t~d~3| ~w~t~d~9| =~t~d~15|~n~n',[Numero7,Op3,Numero8,Numero9]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- style_check(-singleton). % la singola cifra può apparire anche una sola volta

% si chiede solo la prima soluzione trovata, del resto se il problema è ben
% congegnato ne ammette appunto una sola

% problemi generati da encalc2

% DFE -  DG = DIE    195 -  10 = 185
%   +     +     -      +     +     -
% DEH +  DC = DAF    157 +  12 = 169
%   =     =     =      =     =     =
% JEC /  CC =  DA    352 /  22 =  16
test1 :-
	time(calcolo_enigmatico([D,F,E], -,[  D,G],=,[D,I,E],
	                             +,         +,        -,
                            [D,E,H], +,[  D,C],=,[D,A,F],
				     =,         =,        =,
                            [J,E,C],//,[  C,C],=,[  D,A])),
							!.

%  HH *  GA = IFA     66 *  12 = 792
%   +     -     -      +     -     -
% GAB /   B =  GH    128 /   8 =  16
%   =     =     =      =     =     =
% GFC *   C = IIH    194 *   4 = 776
test2 :-
	time(calcolo_enigmatico([  H,H], *,[  G,A],=,[I,F,A],
	                             +,         -,        -,
                            [G,A,B],//,[    B],=,[  G,H],
				     =,         =,        =,
                            [G,F,C], *,[    C],=,[I,I,H])),
							!.

%  IB *  HI = HIB     10 *  51 = 510
%   +     /     -      +     /     -
% GEB -  ID = GAJ    260 -  17 = 243
%   =     =     =      =     =     =
% GDB -   J = GED    270 -   3 = 267
test3 :-
	time(calcolo_enigmatico([  I,B], *,[  H,I],=,[H,I,B],
	                             +,        //,        -,
                            [G,E,B], -,[  I,D],=,[G,A,J],
				     =,         =,        =,
                            [G,D,B], -,[    J],=,[G,E,D])),
							!.

% problemi dalla Settimana Enigmistica

%  AB *  CA = DEF     47 *  14 = 658
%   +     /     /      +     /     /
%  AG /   B =   B     49 /   7 =   7
%   =     =     =      =     =     =
%  GD -   H =  GA     96 -   2 =  94
test4 :-
	time(calcolo_enigmatico([  A,B], *,[  C,A],=,[D,E,F],
	                             +,        //,       //,
                            [  A,G],//,[    B],=,[    B],
				     =,         =,        =,
                            [  G,D], -,[    H],=,[  G,A])),
							!.

%  AA *   B = CDE     33 *   8 = 264
%   +     -     -      +     -     -
% FGB /   C =  HI    158 /   2 =  79
%   =     =     =      =     =     =
% FIF -   D = FBG    191 -   6 = 185
test5 :-
	time(calcolo_enigmatico([  A,A], *,[    B],=,[C,D,E],
	                             +,         -,        -,
                            [F,G,B],//,[    C],=,[  H,I],
				     =,         =,        =,
                            [F,I,F], -,[    D],=,[F,B,G])),
							!.

%  AB +  CD =  EC     34 +  51 =  85
%   *     -     +      *     -     +
%  DF *  AG = BEG     16 *  30 = 480
%   =     =     =      =     =     =
% CBB +  HD = CFC    544 +  21 = 565
test6 :-
	time(calcolo_enigmatico([  A,B], +,[  C,D],=,[  E,C],
	                             *,         -,        +,
                            [  D,F], *,[  A,G],=,[B,E,G],
				     =,         =,        =,
                            [C,B,B], +,[  H,D],=,[C,F,C])),
							!.
