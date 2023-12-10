pere(pepin,charlemagne).
pere(perePapin,papin).
pere(pereBerthe,berthe).
pere(pepin,frereCharlemagne).
pere(pepin,soeurCharlemagne).
pere(grandPerePapin,perePapin).
pere(ariereGrandPerePapin,grandPerePapin).
pere(grandPereBerthe,pereBerthe).
pere(ariereGrandPereBerthe,grandPereBerthe).

mere(berthe,charlemagne).
mere(berthe,frereCharlemagne).
mere(berthe,soeurCharlemagne).

/* 2 */
parent(X,Y) :- pere(X,Y).
parent(X,Y) :- mere(X,Y).

/* 4 */
parent(X,Y,Z) :- pere(X,Z),mere(Y,Z).

/* 5 */
grandPere(X,Y) :- pere(X,Z),pere(Z,Y).

frereOuSoeur(X,Y) :- pere(Z,X),pere(Z,Y), X \== Y.
frereOuSoeur(X,Y) :- mere(Z,X),mere(Z,Y), X \== Y.

/* 6 */
ancetre(X,Y) :- parent(X,Y).
ancetre(X,Y) :- parent(Z,Y),ancetre(X,Z).


/*
  2. parent(X, charlemagne). rend X=pepin et X=berthe
  3.
  parent(charlemagne,X). rend false. charlemagne est pere ni mere de personne dans la base des faits;
  parent(pepin,Y) rend Y=charlemagne. pepin est le pere de charlemagne.
  parent(A,B) rend l'ensemble des couples A,B dont A est parent de B. rend A=pepin,B=charlemagne et A=berthe,B=charlemagne
*/