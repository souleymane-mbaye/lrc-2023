r(a,b).
q(X,X).
q(X,Z) :- r(X,Y), q(Y,Z).

/*
1. Calcul à la main des réponses aux requêtes:
  - q(X,b) => X=b, il cherche dans les faits les faits qui satisfont la requète.
    Quand X=b on a q(b,b) est true d'après la règle q(X,X)
    Quand X=a on a r(a,a),r(a,b) d'après la règle q(X,b)
  - q(b,X) => X=b
    Qand X=b q(b,b) donne true
    Quand X=a l'on pas q(b,a) et non plus r(b,a),r(a,a)

2. Nous avons le résultat escompté.
*/