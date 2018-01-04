not( X ) :- X, !, fail.
not( _ ).


isAirport(Air)  :-
      airport(Air, _T,_,_), ! ; write('Airport not found'), nl, fail.

displayList([]) :- nl.
displayList([Head|Tail]) :- write(Head), write(' '), displayList(Tail).

writepath( [],[] ) :-
   nl.
writepath( [Head1,Head2|Tail],[H1,T1,H2,T2|Tail2] ) :-
   airport(Head1,Name1,_,_),
   airport(Head2,Name2,_,_), 
   write( 'depart  ' ), write(Head1), write('  '), 
   write(Name1), write('  '), write(H1), 
   write(':'),write(T1), nl,  
   write( 'arrive  ' ), write(Head2), write('  '), 
   write(Name2), write('  '), write(H2),
   write(':'),write(T2), nl,
   writepath( Tail,Tail2 ).


fly(Start,End) :- fly_helper(Start,End) 
        , ! ; write('path not found'), nl, fail.

fly_helper(Start,End):-
        isAirport(Start),
        isAirport(End),
        listpath(Start, End, T, -0.5, Sched), 
        writepath(T,Sched).

listpath( Node, End, Outlist, ArrTime, Sched) :-
   listpath( Node, End, [Node], Outlist, ArrTime, Sched).

listpath( Node, Node, _, [], _,[]).
listpath( Node, End, Tried, [Node,Next|List], 
        ArrTime, [H,M,ArrHours,ArrMins|Sched]) :-
   flight( Node, Next, time(H,M)),
   not( member( Next, Tried )),
   toHours(H,M,Result), 
   ArrTime < 24,
   ArrTime + 0.5 < Result,
   distance(Node,Next, Miles),
   timeToDest(500,Miles,TimeRes),
   NextArrTime is Result + TimeRes,
   ArrHours is floor(NextArrTime),
   ArrMins is floor((NextArrTime - floor(NextArrTime))*60),
   listpath( Next, End, [Next|Tried], List, NextArrTime,Sched).

distance(Start,End, Miles) :-
       toRadians(Start, LatRad_1, LongRad_1),
       toRadians(End, LatRad_2, LongRad_2),
       harvesine(LatRad_1, LongRad_1, LatRad_2, LongRad_2, Dist),
       Miles is Dist.

toRadians(Source, LatRad, LongRad) :-
        airport(Source, _, degmin(X_1, Y_1 ), degmin(X_2, Y_2)),
        radians(X_1,Y_1,Res),
        radians(X_2,Y_2,Res2),
        LatRad is Res, LongRad is Res2.


radians(Deg,Min,Res) :- Res is (Deg + Min/60).


harvesine(Lat1, Lon1, Lat2, Lon2, Dis) :-
        P is 0.017453292519943295,
        A is (0.5 - cos((Lat2 - Lat1) * P) / 2 + 
        cos(Lat1 * P) * cos(Lat2 * P) * 
        (1 - cos((Lon2 - Lon1) * P)) / 2),
        Dis is (2* 3959 * asin(sqrt(A))).

timeToDest(Speed,Distance, Res) :- Res is Distance/Speed.

toHours(Hours, Minutes, Res) :- Res is (Hours + Minutes/60).  
