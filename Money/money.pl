% risolve il problema di criptoaritmetica
%
%  SEND+MORE=MONEY
%
% dal libro Knowledge System and Prolog di Walker, McCord, Sowa, Wilson

% pagina 93 paragrafo 2.4.2. Generate and Test

pick(X,[X|TL],TL).
pick(X,[HD|TL],[HD|REM]):-pick(X,TL,REM).

solve1([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]) :-
 pick(M,[0,1,2,3,4,5,6,7,8,9],R9),
   pick(S,R9,R8),
   pick(O,R8,R7),pick(E,R7,R6),pick(N,R6,R5),
   pick(R,R5,R4),pick(D,R4,R3),pick(Y,R3,_R2),
   M>0,(C4=0;C4=1),M=C4,S>0,
   (C3=0;C3=1),
   O is S+M+C3-10*C4,
   (C2=0;C2=1),
   N is E+O+C2-10*C3,
   (C1=0;C1=1),
   R is 10*C2+E-N-C1,
   Y is D+E-10*C1.

% pagina 95 paragrafo 2.4.3. Reordering the Generate and Test

solve2([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]) :-
   M=1,C4=1,
   R9=[0,2,3,4,5,6,7,8,9],
   pick(S,R9,R8),
   S>0,(C3=0;C3=1),
   O is S+M+C3-10*C4,
   pick(O,R8,R7),
   pick(E,R7,R6),(C2=0;C2=1),
   N is E+O+C2-10*C3,
   pick(N,R6,R5),
   (C1=0;C1=1),
   R is 10*C2+E-N-C1,
   pick(R,R5,R4),
   pick(D,R4,R3),
   Y is D+E-10*C1,
   pick(Y,R3,_R2).

% Un minimo di statistica ed interfaccia utente

goal1:-nl,time(solve1(SEND,MORE,MONEY)),nl,
  write(' SEND+    '),write(SEND),writeln('+'),
  write(' MORE=    '),write(MORE),writeln('='),
  writeln('-----  ---------------'),
  write('MONEY  '),writeln(MONEY).

goal2:-nl,time(solve2(SEND,MORE,MONEY)),nl,
  write(' SEND+    '),write(SEND),writeln('+'),
  write(' MORE=    '),write(MORE),writeln('='),
  writeln('-----  ---------------'),
  write('MONEY  '),writeln(MONEY).