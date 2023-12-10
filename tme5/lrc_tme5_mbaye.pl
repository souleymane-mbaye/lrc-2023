/*Exo 2*/
/* T-BOX */
subs(chat,felin).                                    /* ligne 3 Les chats sont des felins*/
subs(felin,mammifere).
subs(mammifere,animal).
subs(canide,mammifere).
subs(chien,canide).
subs(canide,chien).
subs(canari,animal).
subs(animal,etreVivant).
subs(lion,felin).
subs(lion,carnivoreExc).
subs(carnivoreExc,predateur).
subs(chihuahua,and(chien,pet)).                      /* ligne 14 chihuahua est à la fois un chien et un animal de compagnie*/
subs(souris,mammifere).
subs(and(animal,some(aMaitre)),pet).                 /* ligne 16 un animal qui possede un maitre est un animal de compagnie*/
subs(pet,some(aMaitre)).
subs(animal,some(mange)).
subs(some(aMaitre),all(aMaitre,personne)).           /* ligne 19 toute entit ́e qui a maitre ne peut avoir qu’un (ou plusieurs) maitre(s) humain(s) */
subs(and(animal,plante),nothing).
subs(and(all(mange,nothing),some(mange)),nothing).   /* ligne 21 On ne peut pas a la fois ne rien manger (ne manger que des choses qui n’existent pas) et manger quelque chose */
equiv(carnivoreExc,all(mange,animal)).               /* ligne 22 Un carnivore exclusif est d ́efini comme une entit ́e qui mange uniquement des animaux */
equiv(herbivoreExc,all(mange,plante)).


subsS1(C,C).
subsS1(C,D):-subs(C,D),C\==D.
subsS1(C,D):-subs(C,E),subsS1(E,D).

/* 1 
La premiere traduit toute chose est subsumée par elle meme.
La deuxieme C est subsumée par D, si on la directement defini dans la base de connaissances.
La troisieme C est subsumee par D, si on a une subsomption defini dans la base de connaissance de C et E qui est un
element quelconque. Et il existe récursivement une subsomption de par D.
*/

/* subsS1(canari,animal). rend true;*/
/* subsS1(chat,etreVivant). rend true;*/

/* 2
   subsS1(chien,souris). le programme ne s'arrete, une genre de récursion infinie.
   il y a les mêmes paramètres qui réapparaissent dans la récursion. Ce qui la rend infinie.*/

subsS(C,D) :- subsS(C,D,[C]).
subsS(C,C,_).
subsS(C,D,_):-subs(C,D),C\==D.
subsS(C,D,L):-subs(C,E),not(member(E,L)),subsS(E,D,[E|L]),E\==D.
subsS(A,B) :- equiv(A,B).
subsS(A,B) :- equiv(B,A).

/* 3
   subsS(chien,canide). rend true plusieurs fois il existe plusieurs maniere de montrer la subsomption.*/
/* subsS(chien,chien). rend true; donc vraie*/
/* subsS(chien,souris). rend true ;
Il trouve une subsomption de chien vers canide. Mais n'en trouve pas une de canide vers souris
*/

/* 4
    subsS(souris,some(mange)). rend true; donc vraie
    souris est un mamifere, un mamifere est un animal, un animal mange mange quelque chose.
*/

/* 5
    subsS(chat, X). renvoie tout ce que chat peut etre subsumé dans la base des connaissances. L'ensemble des 
    elements tels que subsS(chat, el) est true.
    subsS(X, mamifere). renvoie tout ce qui peut etre subsume par un mamifere dans la base des connaissances. 
    L'ensemble des elements tels que subsS(el,mamifer) renvoie true.

    subsS(chat,X). renvoie :
      X = chat ;
      X = felin ;
      X = mammifere ;
      X = animal ;
      X = etreVivant ;
      X = some(mange) ;
      false.
    subsS(X,mamifere). renvoie X = mamifere.
*/

/* 6 

subsS(A,B) :- equiv(A,B).
subsS(A,B) :- equiv(B,A).*/

/* Avant
    subsS(lion,all(mange,animal)). rend false.
   Apres
    subsS(lion,all(mange,animal)). rend false.
   */