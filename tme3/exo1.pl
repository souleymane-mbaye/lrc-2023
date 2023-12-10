r(a,b).
r(f(X),Y) :- p(X,Y).
p(f(X),Y) :- r(X,Y).

/*
1. Réponse aux requêtes:
  -r(f(f(a)),b) = p(f(a),b) = r(a,b) = true
  -p(f(a),b)    = r(a,b) = true

2. Le test avec prolog nous rend true pour les requetes.
*/