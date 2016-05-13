/*---------------------------------------------------------------
xreverse(L, R) reverses a list, 
where L is a given list and R is either a variable or another given list.
Test Cases:
	xreverse([7,3,4],[4,3,7]) => true
	xreverse([7,3,4],[4,3,5]) => false
	xreverse([7,3,4], R) 	  => R = [4,3,7]
---------------------------------------------------------------*/

xreverse([],[]).
xreverse([H|TL], R) :- 
	xreverse(TL, T), append(T, [H], R).

/*---------------------------------------------------------------
xunique(L, Lu) checks for unique list.
where L is a given list of atoms and Lu is a copy of L where all the 
duplicates have been removed. 
Lu can be either a variable, or a given list. 
The elements of Lu should be in the order in which they first appear in L.
Test Cases:
	xunique([a,c,a,d], L)            			   => L = [a,c,d]
	xunique([a,c,a,d], [a,c,d])    				   => true
	xunique([a,c,a,d], [c,a,d])   				   => false. (because of wrong order)
	xunique([a,a,a,a,a,b,b,b,b,b,c,c,c,c,b,a], L)  => L = [a,b,c]
	xunique([], L) 								   => L = []
---------------------------------------------------------------*/

xunique([], []).
xunique([H|T], [H|TL]) :- 
    dups(H, T, TL2), xunique(TL2, TL).

dups(_, [], []).
dups(X, [X|T], TL) :- 
    dups(X, T, TL).
dups(X, [H|T], [H|TL]) :-
    X \= H, dups(X, T, TL).

/*---------------------------------------------------------------
xunion(L1, L2, L) 
where L1 and L2 are given lists of atoms, and L contains 
the unique elements that are contained in both L1 and L2. 
L should contain the unique elements of L1 (in the same order as in L1) 
followed by the unique elements of L2 that are not contained in L1 
(in the same order as in L2). There should be no redundancy in L. 
The predicate should work both if L is a variable and if L is a given list. 
Test Cases:
	xunion([a,c,a,d], [b,a,c], L) => L = [a,c,d,b], 
	xunion([a,c,d], [b,a,c], [a,c,d,b]) => true, 
	xunion([a,c,d], [b,a,c], [a,c,d,b,a]) => false.
---------------------------------------------------------------*/

xunion(L1, L2, L) :- 
    append(L1, L2, X), xunique(X, L).

/*---------------------------------------------------------------
removeLast(L, L1, Last) 
where L is a given nonempty list, L1 is the result of removing the last element 
from L, and Last is that last element. L1 and Last can be either variables, 
or given values.
Test Cases:
	removeLast([a,c,a,d], L1, Last) => L1 = [a,c,a], Last = d
	removeLast([a,c,a,d], L1, d)    => L1 = [a,c,a]
	removeLast([a,c,a,d], L1, [d])  => false
	removeLast([a], L1, Last)       => L1 = [], Last = a
	removeLast([[a,b,c]], L1, Last) => L1 = [], Last = [a,b,c]
---------------------------------------------------------------*/

removeLast([H|T], T, H) :- 
	T = [].
removeLast([H|T], [H|L1], Last) :- 
	removeLast(T, L1, Last).

/*---------------------------------------------------------------
The clique problem is a graph-theoretic problem of finding a subset of nodes 
where each node is connected to every other node in the subset. 
In this problem a graph will be represented by a collection of predicates, 
node(A) and edge(A,B) where A and B are constants. Edges are undirected but only 
written once, so edge(A,B) also implies edge(B,A).
For example, the following set of predicates represents a graph:
	node(a).
	node(b).
	node(c).
	node(d).
	node(e).

	edge(a,b).
	edge(b,c).
	edge(c,a).
	edge(d,a).
	edge(a,e).
The set of nodes [a,b,c] is a clique, and so is every subset of it 
such as [a,c] or [b]. The set [a,d] and its subsets [a] and [d] also a clique, etc. 
The empty set [] is also a clique.
---------------------------------------------------------------*/

clique(L) :- 
	findall(X,node(X),Nodes), subset(L,Nodes), allConnected(L).

subset([], _).
subset([X|Xs], Set) :-
	aappend(_, [X|Set1], Set), subset(Xs, Set1).

aappend([], L, L).
aappend([H|T], L, [H|R]) :-
	aappend(T, L, R).

/*---------------------------------------------------------------
allConnected(L) to test if each node in L is connected to each other node in L. 
A node A is connected to another node B if either edge(A,B) or edge(B,A) is true.
allConnected(L) is true for an empty list, L= []. The recursive case is: 
allConnected([A|L]) if A is connected to every node in L and allConnected(L). 
Thus, you need to define a predicate, say connect(A,L), to test if A is connected to every node in L.
Test Cases:
	allConnected([a,b])   => true
	allConnected([a,b,c]) => true
	allConnected([a,c])   => true
	allConnected([e,c])   => false
	allConnected([d,c])   => false

---------------------------------------------------------------*/

allConnected([]).
allConnected([H|T]) :- 
    connect(H, T), allConnected(T).

connect(_, []).
connect(A, [TL|L]) :- 
    edge(A, TL), connect(A, L).
connect(A, [TL|L]) :- 
    edge(TL, A), connect(A, L).
