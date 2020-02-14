parent(kim,holly).
parent(margaret,kim).
parent(margaret,kent).
parent(esther,margaret).
parent(herbert,margarent).
parent(herbert,jean).
grandparent(GP,GC):-
	parent(GP,P),parent(P,GC).
greatgrandparent(GGP,GGC):-
	parent(GGP,GP),grandparent(GP,GGC).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(Z,Y),ancestor(X,Z).
