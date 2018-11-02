% Four bands, each from a different side of town, marched in a parade. Each
% band played only one piece, and no two bands played the same piece. From
% the following clues, determine the order in which the bands marched an the
% pieces they played.
%
% 1.  The band from the north side was at the head of the parade.
%
% 2.  "American Patrol" was the second piece played.
%
% 3.  The band from the east or west side played "Yanke Doodle."
%
% 4.  The last band played "When the Saints Go Marching in," just behind the
%     band from the west side.
%
% 5.  The bands that palyed "American Patrol" and "Stars and Stripes
%     Forever" are from opposite sides of town.
%
% dal libro Knowledge System and Prolog di Walker, McCord, Sowa, Wilson

% pagina 66 paragrafo 2.2.7. Reversible Predicates

opp(north,south). opp(south,north).
opp(east,west).   opp(west,east).

bands(L):-L=[[1,_,_],[2,_,_],[3,_,_],[4,_,_]],
   member([1,north,_],L),
   member([2,_,'American Patrol'],L),
   member([_,S1,'Yankee Doodle'],L),
   (S1=east;S1=west),
   member([4,_,'When the Saints Go Marching in'],L),
   member([3,west,_],L),
   member([_,S2,'American Patrol'],L),
   member([_,S3,'Stars and Stripes Forever'],L),
   opp(S2,S3),
   member([_,north,_],L),member([_,south,_],L),
   member([_,east,_],L),member([_,west,_],L).