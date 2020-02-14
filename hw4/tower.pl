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
solve_left(Size,Row,Count) :-
	fd_all_different(Row),row_less_than(Size,Row),left_count(Row,Count).
/* variables: size, row, counts for row */
/* check count from right */
right_count([],X,X,_).
right_count([H|T],Count,Acc,Last) :-
	H#<#Last,left_count(T,Count,Acc,Last).
right_count([H|T],Count,Acc,Last) :-
	H#>#Last,Plus is Acc+1,left_count(T,Count,Plus,H).
right_count(Row,Count) :-
	reverse(Row,Reversed),left_count(Reversed,Count,0,0).
/* variables: Row, Count */
/* solve right */
solve_right(Size,Row,Count) :-
	fd_all_different(Row),row_less_than(Size,Row),right_count(Row,Count).
/* variables: size, row, counts for row */
/* solve row */
solve_row(Size,Row,Counts) :-
	length(Row,Size),length(Counts,2),fd_element_var(1,Counts,Left),solve_left(Size,Row,Left),fd_element_var(2,Counts,Right),solve_right(Size,Row,Right).
/* variables: size, row, counts for row */
/* solve rows */
solve_rows(_,[],[[],[]],0).
solve_rows(Size,[H|T],[[HL|TL],[HR|TR]],0) :-
	solve_row(Size,H,[HL,HR]),solve_rows(Size,T,[TL,TR],0).
solve_rows(Size,Array,Counts) :-
	length(Array,Size),solve_rows(Size,Array,Counts,0).
/* variables: size, array, counts */

/* convert rows to columns */
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
	lists_firsts_rests(Rest, Fs, Oss).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
	lists_firsts_rests(Ms, Ts, Ms1),
	transpose(Rs, Ms1, Tss).
transpose([], []).
transpose([F|Fs], Ts) :-
	transpose(F, [F|Fs], Ts).
/* variables: array, output */
/* solve columns */
solve_columns(Size,Array,Counts) :-
	length(Array,Size),transpose(Array,Transposed),solve_rows(Size,Transposed,Counts).
/* variables: size, array, counts */
/* make sure rows and columns solutions match */
/* base case, no elements */
tower(N,T,C) :- N=0,T=[],C=counts([],[],[],[]).
tower(N,T,C) :-
	C=counts(Top,Bottom,Left,Right),solve_rows(N,T,[Left,Right]),solve_columns(N,T,[Top,Bottom]).

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
plain_tower(N,T,C) :- N=0,T=[],C=counts([],[],[],[]).

ambiguous(N,C,T1,T2) :- tower(N,T1,C),tower(N,T2,C),T1\==T2.

tower_test([[2,3,4,5,1],
[5,4,1,3,2],
[4,1,5,2,3],
[1,2,3,4,5],
[3,5,2,1,4]]).

/* returns false, because tower and plain_tower not implemented */
run_tower(Time) :- tower(5,tower_test,_),statistics(runtime, [_,Time]).
run_plain_tower(Time) :- plain_tower(5,tower_test,_),statistics(runtime, [_,Time]).
speedup(Val) :- statistics(runtime, [_,_]),run_tower(Time1),run_plain_tower(Time2),Val is Time2/Time1.
