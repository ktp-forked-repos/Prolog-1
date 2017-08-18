% Cambi per un euro
% Cinquanta  = 50 ¢
% Venti      = 20 ¢
% Dieci      = 10 ¢
% Cinque     =  5 ¢
% Due        =  2 ¢
% Uno        =  1 ¢
%
% La risposta (4562) è il coefficente di x^100 nella funzione generatrice
%
%   1        1         1         1          1          1
% ----- * ------- * ------- * -------- * -------- * --------
% (1-x)   (1-x^2)   (1-x^5)   (1-x^10)   (1-x^20)   (1-x^50)

cambio(Cinquanta, Venti, Dieci, Cinque, Due, Uno) :-
	between(0, 2,Cinquanta),
	between(0, 5,Venti),
	between(0,10,Dieci),
	between(0,20,Cinque),
	between(0,50,Due),
	Euro is 50 * Cinquanta + 20 * Venti + 10 * Dieci + 5 * Cinque + 2 * Due,
	Euro =< 100,
	Uno is 100 - Euro.

test :- nb_setval(cnt,1), testx.

testx :-
    cambio(Cinquanta,Venti,Dieci,Cinque,Due,Uno),
    nb_getval(cnt,CNT),
    NewCNT is CNT+1,
    nb_setval(cnt,NewCNT),
    writef('%5r --%2r Cinquanta,%2r Venti,%3r Dieci,%3r Cinque,%3r Due,%4r Uno\n',[CNT,Cinquanta,Venti,Dieci,Cinque,Due,Uno]),
    fail.