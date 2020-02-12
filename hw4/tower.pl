tower(N,T,C) :- N=0,T=[],C=counts([],[],[],[]).
plain_tower(N,T,C) :- N=0,T=[],C=counts([],[],[],[]).
tower_test([[2,3,4,5,1],
[5,4,1,3,2],
[4,1,5,2,3],
[1,2,3,4,5],
[3,5,2,1,4]]).
/* returns false, because tower and plain_tower not implemented */
run_tower(Time) :- tower(5,tower_test,_),statistics(runtime, [_,Time]).
run_plain_tower(Time) :- plain_tower(5,tower_test,_),statistics(runtime, [_,Time]).
speedup(Val) :- statistics(runtime, [_,_]),run_tower(Time1),run_plain_tower(Time2),Val is Time2/Time1.
