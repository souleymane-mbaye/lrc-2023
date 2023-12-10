revise(E) :- serieux(E).
faitDevoirsLendemain(E) :- consciencieux(E).
reussit(E) :- revise(E).
serieux(E) :- faitDevoirsLendemain(E).
consciencieux(pascal).
consciencieux(zoe).

/*
2. Requête à la question << qui va réussir >>:
  reussit(E).
  Pascal et Zoe vont réussir.
3.
 reussit(E) verifie revise(E) qui verifie serieux(E) qui verifie faitDevoirsLendemain(E) qui verifie consciencieux(E) qui trouvent les faits:
 consciencieux(pascal) et consciencieux(zoe).
*/