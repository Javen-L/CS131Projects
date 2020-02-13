/* base case, no elements */
tower(N,T,C) :- N=0,T=[],C=counts([],[],[],[]).

/* check row same */
rows_equal(R1,R2) :- R1==R2.
/* variables: row1, row2 */
/* check less than or equal to Size and greater than 0*/
row_less_than(_,[]).
row_less_than(Size,[H|T]) :-
	H#=<#Size,H#>#0,row_less_than(Size,T).
/* variables: Size, Row */
/* check count from left */
left_count([],X,X,_).
left_count([H|T],Count,Acc,Last) :-
	H#<#Last,left_count(T,Count,Acc,Last).
left_count([H|T],Count,Acc,Last) :-
	H#>#Last,Plus is Acc+1,left_count(T,Count,Plus,H).
left_count(Row,Count) :-
	left_count(Row,Count,0,0).
/* variables: Row, Count*/
/* solve left */
solve_left(0,[],0).
solve_left(Size,Row,Count) :-
	length(Row,Size),fd_all_different(Row),row_less_than(Size,Row),left_count(Row,Count).
/* variables: size, row, counts for row */
/* solve right */
/* variables: size, row, counts for row */
/* solve row */
/* variables: size, row, counts for row */
/* solve rows */
/* variables: size, array, counts */

/* check column same */
/* variables: size, column number, array, boolean */
/* solve up */
/* variables: size, column number, array, counts */
/* solve down */
/* variables: size, column number, array, counts */
/* solve column */
/* variables: size, column number, array, counts */
/* solve columns */
/* variables: size, array, counts */

/* make sure rows and columns solutions match */

plain_tower(N,T,C) :- N=0,T=[],C=counts([],[],[],[]).
/* solve left */
/* variables: size, row, counts for row */
/* solve right */
/* variables: size, row, counts for row */
/* solve row */
/* variables: size, row, counts for row */
/* solve rows */
/* variables: size, array, counts */
/* solve up */
/* variables: size, column number, array, counts */
/* solve down */
/* variables: size, column number, array, counts */
/* solve column */
/* variables: size, column number, array, counts */
/* solve columns */
/* variables: size, array, counts */
/* make sure rows and columns solutions match */

ambiguous(N,C,T1,T2) :- tower(N,T1,C),tower(N,T2,C),T1\=T2.

tower_test([[2,3,4,5,1],
[5,4,1,3,2],
[4,1,5,2,3],
[1,2,3,4,5],
[3,5,2,1,4]]).

/* returns false, because tower and plain_tower not implemented */
run_tower(Time) :- tower(5,tower_test,_),statistics(runtime, [_,Time]).
run_plain_tower(Time) :- plain_tower(5,tower_test,_),statistics(runtime, [_,Time]).
speedup(Val) :- statistics(runtime, [_,_]),run_tower(Time1),run_plain_tower(Time2),Val is Time2/Time1.
