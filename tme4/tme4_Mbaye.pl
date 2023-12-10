/*
Exercice 1:

1. ?- [a,[b,c],d] = [X]. rend false
2. ?- [a,[b,c],d] = [X,Y,Z]. rend X=a, Y=[b,c], Z=[d]
3. ?- [a,[b,c],d] = [a|L]. rend L=[[b,c],d]
4. ?- [a,[b,c],d] = [X,Y]. rend false


Exercice 2:

1. Concatenation */

concatene([],L,L).
concatene([X|L1],L2,[X|L]) :- concatene(L1,L2,L).

/* concatene([a,b,c],[d],L2). rend L2=[a,b,c,d] */

inverse([],[]).
inverse([X|L1],L) :- 
  inverse(L1,IL1),
  concatene(IL1,[X],L).

/* inverse([a,b,c,d],L2). rend L2 = [d, c, b, a]. */

supprime([],_,[]).
supprime([X|L1],X,L2) :- supprime(L1,X,L2).
supprime([X|L1],Y,L3) :- X \== Y,
  supprime(L1,Y,L2),
  concatene([X],L2,L3).

/* supprime([a,b,a,c],a,L). rend L = [b, c] ; false */

filtre([],_,[]).
filtre(L1,[],L1).
filtre(L1,[X|L2],L4) :- 
  supprime(L1,X,L3),
  filtre(L3,L2,L4).

/* filtre([1,2,3,4,2,3,4,2,4,1],[2,4],L). rend L = [1, 3, 3, 1]; false */

/* Exercice 3 */

palindrome(X) :-
  inverse(X,L),
  X == L.

/* palindrome([l,a,v,a,l]). rend true ; false. 
   palindrome([n,a,v,a,l]). rend false. */

palindrome([],[]).
palindrome([X],[X]).
palindrome([X|L1],[Y|L2]) :- 
  X == Y,
  palindrome(L1,L2).

