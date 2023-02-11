% Week 1 - The Basics

/* Basics of Prolog:
 *
 * The building blocks of logic programming are called Facts, Rules, and Queries.
 * FACTS:
 *  These are statements that we have to consider as true.
 *  Following are some syntax rules of Facts:
 *      Names of properties/relationships begin with lower case letters.
 *      The relationship name appears as the first term.
 *      Objects appear as comma separated arguments within parentheses.
 *          These must start with lower case letters, digits, and can be a 
 *          string of characters.
 *      A full-stop "." must end a fact.
 *      
 * RULES:
 * These represent implicit relationships between objects - i.e. the LHS is true
 *  when the RHS is true.
 * 
 * QUERIES:
 * These are some questions (represented by ?- at the start) on the relationships
 *  between objects and object properties. It will use the Knowledge Base, which
 *  is a collection of Facts and Rules.
*/

/* Examples:
?- likes(sam,dahl).
	true
?- likes(sam,chop_suey).
	true
?- likes(sam,pizza).
	true
?- likes(sam,chips).
	true
?- likes(sam,curry).
	false (curry is indian but not mild)
*/

% these are rules
likes(sam,Food) :-
    indian(Food),
    mild(Food).
likes(sam,Food) :-
    chinese(Food).
likes(sam,Food) :-
    italian(Food).
likes(sam,chips).

% these are facts, which represent explicit relationships between objects
% 	this means they are unconditionally true in nature
indian(curry).
indian(dahl).
indian(tandoori).
indian(kurma).

mild(dahl).
mild(tandoori).
mild(kurma).

chinese(chow_mein).
chinese(chop_suey).
chinese(sweet_and_sour).

italian(pizza).
italian(spaghetti).

% Week 1 Seminar Q6

/* Q: What do these return?
?- animal(bear,mammal).
	true
?- animal(elephant,mammal).
	false
?- animal(bear,X).
	X = mammal
?- animal(X,bird).
	X = sparrow, then X = stork
?- animal(X,Y).
	displays everything in order
*/

animal(bear,mammal).
animal(tiger,mammal).
animal(sparrow,bird).
animal(stork,bird).

% Week 2 Seminar (nothing new in lectures)

% add one more mammal, one more bird, and 3 fish to Week 1's animals
animal2(bear,mammal).
animal2(tiger,mammal).
animal2(cow,mammal).
animal2(sparrow,bird).
animal2(stork,bird).
animal2(swan,bird).
animal2(carp,fish).
animal2(eel,fish).
animal2(shark,fish).

% define predicates carnivore/1 and herbivore/1 for each of these
carnivore(bear).
carnivore(tiger).
carnivore(stork).
carnivore(eel).
carnivore(shark).
herbivore(cow).
herbivore(sparrow).
herbivore(swan).
herbivore(carp).

% Variables are denoted by a capital starting letter, so X below is a variable

% define predicate has_feathers/1 which classifies all birds having feathers
has_feathers(X):-animal2(X,bird).

% define predicate can_swim/1 which classifies all fish being able to swim
% and maybe one or two of your animals.
can_swim(X):-animal2(X,fish).
can_swim(swan).

% Week 3 - Arithmetic and Recursion

/* Arithmetic:
 * 
 * Basic arithmetic operators: +, -, *, /
 * Basic functions: sin(X), cos(X), abs(X), sqrt(X), max(X,Y)
 * Numerical comparison operators =:=, =\=, >, <, >=, =<
 * is/2 predicate: assign numerical value of right hand side to left hand side 
 * 		(variable assignment)
 * = unification operator: bind free variables to make them match the other members
 * 
 * Recursion:
 * When working with recursion, don't forget to do the base case!
*/

% compute the sum of the first N numbers
sum(0,0).
sum(N,S):- N >= 1,
    M is N - 1,
    sum(M,T),
    S is T + N.

% compute the binomial coefficients
binom(_,0,1).
binom(N,N,1).
binom(N,K,B):- K >= 1 ,
    M is N - 1,
    L is K - 1,
    binom(M,K,B1),
    binom(M,L,B2),
    B is B1 + B2.

% Week 3 Seminar Questions

/* Q3: What does p(T,b) and p2(T,b) return and why?
?- p(T,b).
	yields T=a as the first result.
	The query p(T,b) matches the left-hand side of the first line of the program, with 
    the unifications X=T,Z=b. The right-hand side of this rule is evaluated, yielding 
    the queries q(T,Y) and p(Y,b), in that order. The first query q(T,Y) matches the 
    left-hand side of line 158, with the unifications T=a,Y=b. 
    With these unifications, the second query becomes p(b,b). This query matches the 
    left-hand side of the first rule with the unifications X=b,Z=b. The right-hand side 
    of this rule is evaluated, yielding the queries q(b,Y) and p(Y,b). There is no match 
    for the first query, so this branch fails. Backtracking tries to match the query 
    p(b,b) to line 159 (instead of matching to the first line, which failed), and 
    succeeds. The first successful branch of the evaluation therefore yields T=a.
?- p2(T,b).
	The query p2(T,b) runs into an infinite loop and does not terminate (stack overflow). 
    This can be checked by switching on tracing using the command `trace.` before 
    running the query. 
    The reason is the following: The query p2(T,b) matches the left-hand side of line 163, 
    with the unifications X=T,Z=b. The right-hand side of this rule is evaluated,
	yielding the queries p2(Y,b) and q2(T,Y), in that order. The first query p2(Y,b) 
    matches the left-hand side of line 163, with the unifications X=Y,Z=b. The right-hand
	side of this rule is evaluated, yielding the queries p2(Y’,b) and 2q(Y,Y’) 
    (here Y’ is a new local copy of the variable Y). Continuing this, we see 
    that the program runs into an infinite loop, as it keeps branching into the 
    first rule in the first line of the program.
*/

p(X,Z):-q(X,Y), p(Y,Z).
p(X,X).
q(a,b).

p2(X,Z):-p2(Y,Z), q2(X,Y).
p2(X,X).
q2(a,b).

/* Q4: What is the result of the queries f(3,M) and f(4,M) for the given Prolog program? 
?- f(3,M).
	f(3,M) matches LHS of line XXX, and unifies X=3 and M=M. 3 > 1, and we let Y be
    X-1, so Y is 2. Then we call f(Y, T), where Y is 2. f(2,T) matches LHS of line XXX,
    and unifies X=2 and M=T. 2 > 1 , then we let Y be X - 1, and hence Y is 1. Then we
    call f(Y,T') where Y is 1 and T' is just a local copy of T. f(1,T') matches line
    XXX, so it unifies T'=1.  We then backtrack into f(2,T), where T will be 1-T', so 
    T is 0. We backtrack again into f(3,M), and now M is 1-T, so M is 1.
    f(3,M) returns M=1.
?- f(4,M).
	with similar logic, f(4,M) returns M=0.
    
 * What does the function f compute?
 * 	The function f(X,Y) computes the parity of X, where Y=0 if X is even, 
 * 	and Y=1 if X is odd.
*/

f(0,0).
f(1,1).
f(X,M):- X > 1,
    Y is X-1,
    f(Y,T),
    M is 1-T.

/* Q5: Write fac/2 that returns the factorial of a number recursively. */

fac(1, 1).
fac(X, Y):- X > 0,
    X1 is X - 1,
    fac(X1, Y1),
    Y is Y1 * X.

% Week 4

/* Lists:
 * 
 * List notation [a,b,c]
 * Lists in lists [a,b,[c,d,e]]
 * Access first element of list and the rest by [Head|Tail]
 * Can be iterated: [H1|[H2|Tail]]=[H1,H2|Tail]
 * Test membership: member(El,List)
 * Get nth entry (1-based): nth1(Idx,List,El)
 * 		Idx starts indexing from 1, so nth1(1,[0,1,2],0). returns true
 * Placeholder for arbitrary expression: _
 * Prolog cut operator: !
*/

% print the entire list
write_list([]).
write_list([H|T]):-
 write(H), nl, write_list(T). %nl is new line
 
% count the number of elements in a list
count([],0).
count([_|T],S):-count(T,R), S is R+1.

% take sum of all elements of a list
sumL([],0).
sumL([X|T],S):-sumL(T,R),S is R+X.

% check if element is contained in a list
% also exists as a built-in predicate
member(X,[X|_]).
member(X,[_|T]):-member(X,T).

% get an element at a position
get(1,[X|_],X).
get(I,[_|T],X):-I>1, J is I-1, get(J,T,X).

% determine the index of an element in a list
index(X,[X|_],1).
index(X,[_|Tail],I):-index(X,Tail,P), I is P+1.

% insert an item at beginning of list
prepend(X,L,[X|L]).

% insert an item at end of list
%		to append two lists however, use the built in append/3
%			append([1,2], [3], [1,2,3]). returns true
appendL(X,[],[X]).
appendL(X,[H|T],[H|TP]):-appendL(X,T,TP).

% remove first element of list
drop([_|T],T).

% remove last element of list
most([_],[]).
most([H|T],[H|TP]):-most(T,TP).

% Week 4 Seminar

/* Q2: Towers of Hanoi in Prolog.
 * Write a prolog predicate hanoi/4 where this displays a recursive solution to this
 * problem. The first argument should be the number of disks n, the next three
 * arguments should be the indices of the three pegs which are involved a sequence
 * of moves: the peg from which we move, the peg to which we move, and the peg that
 * is used as intermediate storage.
 * So hanoi(3,1,3,2) would return the solution of 3 moving disks starting at peg 1
 * to peg 3, using peg 2 as an intermediate.
*/

hanoi(1,X,Y,_):-
write(X),
write('-->'),
write(Y),
nl.

hanoi(N,X,Y,Z):-
N>1,
M is N-1,
hanoi(M,X,Z,Y),
hanoi(1,X,Y,_),
hanoi(M,Z,Y,X).

/* Q3 is incredibly easy so I'll leave it as an exercise. */

/* Q4: Consider the Prolog Program p([a,b,c],a(b)). Which answers do the 
 * following queries yield?
?- p(X,Y).
	X = [a,b,c], Y = a(b)
?- p(X,a(Y)).
	X = [a,b,c], Y = b
?- p([X,Y],_).
	false
?- p([X|Y],Z).
	X = a, Y = [b,c], Z = a(b).
?- p([X,Y|Z],X). 
	false
?- p([_|[Y|Z]],_).
	Y = b, Z = [c] - note Z will be a list.
?- p([X|Y],a(Y)).
	false
?- p([a,X,Y|Z],a(X)).
	X = b, Y = c, Z = []
?- p(L,a(E)),member(E,L)
	L = [a,b,c], E = b.
*/

/* Q5:
 *  Write a Prolog function first(X,Y) that checks whether X is the first element 
 * of the list Y. 
 * Write a Prolog function last(X,Y) that checks whether X is the last element 
 * of the list Y.
*/

first(X, [X|_]).

last(X, [X]).
last(X, [_|T]):- last(X,T). 

% Week 5 Seminar (nothing new in lectures)

/* Q4: Write a Prolog program rev/2 that reverses a list. */

rev([], []).
rev([H|T], X) :- rev(T, Y), append(Y, [H], X).

/* Q5:
 * Write a Prolog function sublista/2 that takes two lists as input, and checks 
 * whether the second is a not necessarily consecutive sublist of the first.
 * 		So sublista(X, Y) should return if Y is the same as X but with some
 * 		elements removed.
 * Write a Prolog function sublistb/2 that takes two lists as input, and checks 
 * whether the second is a consecutive sublist of the first.
 * 		So sublistb(X, Y) should return if a list is the same but with some
 * 		start and end elements removed. 
 * Write a Prolog function subset/2 that checks whether the second argument is a 
 * subset of the first.
*/

sublista([],[]).
sublista([X|R1],[X|R2]):-sublista(R1,R2).
sublista([_|R1],X):-sublista(R1,X).

sublistb([],[]).
sublistb([X|R1],[X|R2]):-prefix(R1,R2).
sublistb([_|R1],X):-sublistb(R1,X).

prefix(_,[]).
prefix([X|R1],[X|R2]):-prefix(R1,R2).

subset(_,[]).
subset(X,[H|Y]):-member(H,X),subset(X,Y).
