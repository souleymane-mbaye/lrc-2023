

# Partie 1

    /***  auto referance ***/

fichier_autoref(Lequiv,Result) : 
    elle prend un argument qui contient la TBox et renvoye si le fichier contient un concept qui se autoref 
    elle retourne 1 si la TBox est circulaire sinon 0

boucler_sur_equiv(TBox,Init,Result) : 
    elle prend la TBox en premier argument et 2eme argument pour initialiser qu'on a commencé avec 0 qui veut dire que y'a pas d'autoreference
    puis dans Result en renvoye  1 si il est autoref sinon 0

    si notre init il a changer et est devenue 1 (qui veut dire que y'a un autoref) on stop et on renvoye 1 pour Result 
        (pour optimise et ne pas faire de calcul non pertinant)
    si on parcour toute la TBox on renvoye 0 (qui veut dire que ya pas de autoref)

    cette fonction elle appel autoref ref puis reboucle sur le reste de la TBox

autoref(C,X,List,Init,Result) : 
    C (notre concept complexe ), X (sa deffinition) ,List (list qui contient des concept complexe deja visite dans notre parcour en profendeur comme dfs) 
    ,init (c'est le meme init de boucler_sur_equiv) , Result (Result su le fichier est autoref)
    
    si notre init a ete changer et est devenue 1 on arrete directement et renvoye 1 dans Result
    sinon 
        si notre concept est de type non atomique de type 2 concept (ex : c= and(c1,c2) or(..) not(and(...) ) ...) :
            on parcour en profendeur avec autoref sur notre c1 son init si le init principale puis son Result returne devient l'entre de autoref sur 
            notre c2 et son Result est le Result de notre autoref principale
        sinon si notre concept est de type non atomique de type 1 seul concept (ex : not(c) or not(and ) ...) :
            on parcour en profendeur avec autoref sur notre c son init si le init principale son Result est le Result de notre autoref principale  
        sinon si notre concept est atomique : 
            on retourne 0 si notre init n'a pas trouver deja de autoref 
        sinon si notre C==X : 
            on retourne 1 car on pointe directement
        sinon si notre et X est non atomique complexe mais pas de type (and ou somme ou not ...) seulement le nom:
            on verifier si on a deja visite ce concept avec la list List si il existe on retourne 1 directement
            sinon on fait un parcour en profendeur sur X et en le rajoute a List

    /*** concept ***/

concept(TBox,Abi,Abr,Result) :
    recupere tout les concept et toute les relation et tout les individus puis on appel des fonction auxiliere pour verifier le Abi,TBox,Abr
    le meme principale on demare avec 0 le Result d'une fonction devient l'entre d'une autre la derniere fonction appele retourne Result

verife_inst(Lconcept,Liname,Lrname,Abi,init,Result) : fonction qui verife_inst Abi
    Lconcept (contient tout les concept) , Liname (contient les individus declare) , Lrname (contient toute les relation) , Abi (list de type inst(a,c)),
    init (valeur 0 ou 1) , Result resultat renvoye

    si on a deja trouve une ereure (init =1) on retourne directement 1
    sinon si on a tout parcourou on retourne 0
    sinon 
        on recupere tout les roles et concepts utilise on verifier que tout nos concept et roles utilise on les a declare puis reboucle sur le reste de 
        Abi (meme principe deja explique pour Result)

verife_instR(Liname,Lrname,Abr,Init,Result) : fonction qui verifier Abr
    Liname (contient les individus declare) , Lrname (contient toute les relation) , Abr (list de type instR(a, b, R)) , init (valeur 0 ou 1) 
    , Result resultat renvoye

    si on a deja trouve une ereure (init =1) on retourne directement 1
    sinon si on a tout parcourou on retourne 0
    sinon
        verifier que a et b existent (on les a declare) et meme  chose que R puis on reboucle sur le reste de Abr (meme principe pour resultat)

verife_equiv(Lrname,Lconcept,Lcnamena,TBox,Init,Result) : 
    Lrname (contient toute les relation) , TBox (notre TBox) , Lconcept (list qui contient tout les concept) , Lcnamena (list qui contient que des concept complexe) 
    , init (valeur 0 ou 1) , Result resultat renvoye

    si on a deja trouve une ereure (init =1) on retourne directement 1
    sinon si on a tout parcourou on retourne 0
    sinon
        on verifier dabord que notre concept qu'on defini est bien complexe puis on recupere les roles et concept utilise et on verifie que ils existent
        (on les a deja declare) puis on reboucle sur le reste de la TBox

    /*** traitement_Tbox ***/

traitement_Tbox(TBox,new_tbox) :    
    elle prend une TBox et retourne une nouvelle TBox ou chaque complexe non atomique contient que des complexe atomique sous forme nnf

t_b(TBox,Result) : 
    si on a terminer TBox=[] on returne une list vide []
    sinos on appelle un function suit_T_B puis on l'a mit le resultat retourner de cette fonction sur forme nnf et en le rajoute a Result

suit_T_B(C,C) : 
    le principe c'est que on fait un parcour en profendeur de notre arbre sans changer la forme de l'arbre (exception des feuilles) (par exemple
    lorsque on a or(c1,c2) en returne or(nc1,nc2) ou nc1,nc2 ont subit des transforation) tel que lorsque on trouve un concept non atomique on appelle
    on recupere sa deffinition on la mit on la simplifie meme traitement sur sa deffinition avec suit_T_B et on mis dans la feuilles ou y'avais ce 
    concept complexe le resultat retourner avec suit_T_B  

    /***   traitement_Abox    ***/

traitement_Abox(Tbox,Abi,new_abi) : elle recupere la Tbox traite et appelle la fonction simple_Abox_inst

simple_Abox_inst(Tbox,Abi,Result):- 
    je simplifie la definition d'un individus (ex and(c1,c2)) avec la fonctionsuit_4 puis je la mis sous forme nnf puis je reboucle 

suit_4(Tbox,c,nc):- 
    je fais un parcour en profendeur de mon arbre et si la feuilles est un concept complexe je recupere la definition de ce concept
    avec l'argument Tbox sa definition ou  y' a que des concepts atomiques et sous forme nnf et je le mit 

/******************************************************************************************************************************************************/

# patie 2 


acquisition_prop_type1 : elle lit une prop et renvoye Abil

remplacement_concept_1(List,Prop,Result): 
    
    si il a entrer une prop de type inst(a,c)
        on verifie dabord que tout ce que il a utilise il existe avec une fonction deja utilise pour la fonction (predicat) concept
        si faux
            on affiche qu'il doit entrer que des truc qui existe et en reboucle sur le type de prop qu'on veut demontrer
        sinon
            on vas supprimme et ne laisse que des concept atomique et sous nnf  
    sinon
        on affiche qu'on doit entrer ce type et on reboucle sur le type de prop qu'on veut demontrer

acquisition_prop_type2 : elle lit une prop et renvoye Abil

verife_type_2(List,Prop,Result): 
    
    si il a entrer une prop de type and(_,_)
        on verifie dabord que tout ce que il a utilise il existe avec une fonction deja utilise pour la fonction (predicat) concept
        si faux
            on affiche qu'il doit entrer que des truc qui existe et en reboucle sur le type de prop qu'on veut demontrer
        sinon
            on vas supprimme et ne laisse que des concept atomique et sous nnf  
    sinon
        on affiche qu'on doit entrer ce type et on reboucle sur le type de prop qu'on veut demontrer

 
# Partie 3

resolution(Lie,Lpt,Li,Lu,Ls,Abr):
Dans la fonction de résolution, on applique d'abord toutes les règles some(avec la fonction complete_some). Puis toutes les règles ans avec la fonction transformation_ans,  puis all avec deduction_all, puis or avec deduction_or.
Dans chaque de ces dernières on applique la règle correspondante ensuite on test s'il y a un clash avec la fonction test_clash puis on rappelle la fonction de résolution puis poursuivre notre résolution.

evolue_all((I,all(R,C)), [R1|Abr], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) 
cette fonction permet d'appliquer en une seule fois la règle quelque soit à tous les couples possibles d'assertions: (I,all(R,C)) (a,b,R).
