/* check row same */
rows_equal(R1,R2) :- R1==R2.
/* variables: row1, row2 */
/* check less than or equal to Size and greater than 0*/
row_less_than(_,[]).
row_less_than(Size,[H|T]) :-
	fd_domain(H,1,Size),row_less_than(Size,T).
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
right_count(Row,Count) :-
	reverse(Row,Reversed),left_count(Reversed,Count,0,0).
/* variables: Row, Count */
/* solve right */
solve_right(Size,Row,Count) :-
	fd_all_different(Row),row_less_than(Size,Row),right_count(Row,Count).
/* variables: size, row, counts for row */
/* solve row */
solve_row(Size,Row,Counts) :-
	length(Row,Size),fd_element_var(1,Counts,Left),solve_left(Size,Row,Left),fd_element_var(2,Counts,Right),solve_right(Size,Row,Right),fd_labeling(Row).
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

/* less than */
less_than(Less, _, Less).
less_than(Less, Greater, Acc) :-
	Acc2 is Acc+1,Acc2\==Greater,less_than(Less,Greater,Acc2).
less_than(Less, Greater) :-
	Less\==Greater,less_than(Less, Greater, -1).
/* variables: Less, Greater */
/* lists unique */
unique_list([]).
unique_list([H|T]) :-
	\+(member(H,T)),unique_list(T).
/* variables: Array */
/* check less than or equal to Size and greater than 0*/
plain_row_less_than(_,[]).
plain_row_less_than(Size,[H|T]) :-
	less_than(0,H),H==Size,plain_row_less_than(Size,T).
plain_row_less_than(Size,[H|T]) :-
	less_than(0,H),less_than(H,Size),plain_row_less_than(Size,T).
/* variables: Size, Row */
/* check count from left */
plain_left_count([],X,X,_).
plain_left_count([H|T],Count,Acc,Last) :-
	less_than(H,Last),plain_left_count(T,Count,Acc,Last).
plain_left_count([H|T],Count,Acc,Last) :-
	less_than(Last,H),Plus is Acc+1,plain_left_count(T,Count,Plus,H).
plain_left_count(Row,Count) :-
	plain_left_count(Row,Count,0,0).
/* variables: Row, Count*/
/* solve left */
plain_solve_left(Size,Row,Count) :-
	plain_row_less_than(Size,Row),unique_list(Row),plain_left_count(Row,Count).
/* check count from right */
plain_right_count(Row,Count) :-
	reverse(Row,Reversed),plain_left_count(Reversed,Count).
/* variables: Row, Count */
/* solve right */
plain_solve_right(Size,Row,Count) :-
	plain_row_less_than(Size,Row),unique_list(Row),plain_right_count(Row,Count).
/* variables: size, row, counts for row */
/* solve row */
plain_solve_row(Size,Row,[Left,Right]) :-
	length(Row,Size),plain_solve_left(Size,Row,Left),plain_solve_right(Size,Row,Right).
/* variables: size, row, counts for row */
/* solve rows */
plain_solve_rows(_,[],[[],[]],0).
plain_solve_rows(Size,[H|T],[[HL|TL],[HR|TR]],0) :-
	plain_solve_row(Size,H,[HL,HR]),plain_solve_rows(Size,T,[TL,TR],0).
plain_solve_rows(Size,Array,Counts) :-
	length(Array,Size),plain_solve_rows(Size,Array,Counts,0).
/* variables: size, array, counts */
plain_solve_columns(Size,Array,Counts) :-
	length(Array,Size),transpose(Array,Transposed),plain_solve_rows(Size,Transposed,Counts).
/* variables: size, array, counts */
/* make sure rows and columns solutions match */
plain_tower(N,T,C) :- N=0,T=[],C=counts([],[],[],[]).
plain_tower(N,T,C) :-
	C=counts(Top,Bottom,Left,Right),plain_solve_rows(N,T,[Left,Right]),plain_solve_columns(N,T,[Top,Bottom]).

ambiguous(N,C,T1,T2) :- tower(N,T1,C),tower(N,T2,C),T1\==T2.

/* returns false because plain_tower is not implemented yet */
run_tower(Time) :- ambiguous(_,_,_,_),statistics(runtime, [_,Time]).
run_plain_tower(Time) :- plain_tower(N,T1,C),plain_tower(N,T2,C),T1\==T2,statistics(runtime, [_,Time]).
speedup(Val) :- statistics(runtime, [_,_]),run_tower(Time1),run_plain_tower(Time2),Val is Time2/Time1.
