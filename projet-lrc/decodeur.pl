equiv(sculpteur,and(personne,some(aCree,sculpture))).
equiv(auteur,and(personne,some(aEcrit,livre))).
equiv(editeur,and(personne,and(not(some(aEcrit,livre)),some(aEdite,livre)))).
equiv(parent,and(personne,some(aEnfant,anything))).
cnamea(personne).
cnamea(livre).
cnamea(objet).
cnamea(sculpture).
cnamea(anything).
cnamea(nothing).
cnamena(auteur).
cnamena(editeur).
cnamena(sculpteur).
cnamena(parent).
iname(michelAnge).
iname(david).
iname(sonnets).
iname(vinci).
iname(joconde).
rname(aCree).
rname(aEcrit).
rname(aEdite).
rname(aEnfant).
inst(michelAnge,personne).
inst(david,sculpture).
inst(sonnets,livre).
inst(vinci,personne).
inst(joconde,objet).
instR(michelAnge, david, aCree).
instR(michelAnge, sonnets, aEcrit).
instR(vinci, joconde, aCree).

/*






                PROGRAMME PRINCIPAL










*/

compteur(1).
programme :-
        setof((X,Y), inst(X,Y),Abi),setof((X,Y,Z), instR(X,Y,Z),Abr),setof((X,Y), equiv(X,Y),Tbox),
        premiere_etape(Tbox,Abi,Abr), deuxieme_etape(Abi,Abi1,Tbox),print(Abi1), troisieme_etape(Abi1,Abr).


premiere_etape(Tbox,Abi,Abr):- 
                fichier_autoref(Tbox,Result1),concept(Tbox,Abi,Abr,Result2), stop_program([Result1,Result2]).
deuxieme_etape(Abi,Abi1,Tbox) :- saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).
troisieme_etape(Abi,Abr) :-
                            nl,write('Troisieme étape'),nl,
                            nl,write('Abi: '),print(Abi),nl,
                            nl,write('Abr: '),print(Abr),nl,
                            tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
                            nl,write('Lie: '),print(Lie),nl,
                            nl,write('Lpt: '),print(Lpt),nl,
                            nl,write('Li: '),print(Li),nl,
                            nl,write('Lu: '),print(Lu),nl,
                            nl,write('Ls: '),print(Ls),nl,
                            resolution(Lie,Lpt,Li,Lu,Ls,Abr),
                            nl,write('Youpiiiiii, on a demontre la proposition initiale !!!').

/*






                PARTIE 1










*/

/* remarque 1*/
/* le principe c'est parcours par profendur return 0 si ok sinn 1*/

fichier_autoref(Lequiv,Result):- boucler_sur_equiv(Lequiv,0,Result),!.

boucler_sur_equiv(_,1,1):-!.
boucler_sur_equiv([],_,0):-!.
boucler_sur_equiv([(C,X)|Lequiv],Init,Result):-
                autoref(C,X,Init,Result1),boucler_sur_equiv(Lequiv,Result1,Result),!. 

autoref(C,X,Init,Result):-suit_autoref(C,X,[C],Init,Result),!.

suit_autoref(_,_,_,1,1):-!.
suit_autoref(Concept_complexe,or(C1,C2),List,Init,Result):-
                 suit_autoref(Concept_complexe,C1,List,Init,Result1), suit_autoref(Concept_complexe,C2,List,Result1,Result),! .
suit_autoref(Concept_complexe,and(C1,C2),List,Init,Result):- 
                 suit_autoref(Concept_complexe,C1,List,Init,Result1), suit_autoref(Concept_complexe,C2,List,Result1,Result),! .
suit_autoref(Concept_complexe,some(_,C),List,Init,Result):- suit_autoref(Concept_complexe,C,List,Init,Result),!.
suit_autoref(Concept_complexe,all(_,C),List,Init,Result):- suit_autoref(Concept_complexe,C,List,Init,Result),!.
suit_autoref(Concept_complexe,not(or(C1,C2)),List,Init,Result):-
                 suit_autoref(Concept_complexe,C1,List,Init,Result1), suit_autoref(Concept_complexe,C2,List,Result1,Result),! .
suit_autoref(Concept_complexe,not(and(C1,C2)),List,Init,Result):-
                 suit_autoref(Concept_complexe,C1,List,Init,Result1), suit_autoref(Concept_complexe,C2,List,Result1,Result),! .
suit_autoref(Concept_complexe,not(some(_,C)),List,Init,Result):- suit_autoref(Concept_complexe,C,List,Init,Result),!.
suit_autoref(Concept_complexe,not(all(_,C)),List,Init,Result):- suit_autoref(Concept_complexe,C,List,Init,Result),!.
suit_autoref(Concept_complexe,not(C),Init,List,Result):- suit_autoref(Concept_complexe,C,List,Init,Result),!.
suit_autoref(C,C,_,_,1):-!.
suit_autoref(_,C,List,_,1):-
                cnamena(C),appertient_2(C,List,0,0),!.
suit_autoref(_,C,List,Init,Result):-
                cnamena(C),appertient_2(C,List,0,1),equiv(C,X) 
                ,concat(List,[C],New_List), suit_autoref(C,X,New_List,Init,Result),!.
suit_autoref(_,C,_,0,0):- cnamea(C),!.

/* remarque 2*/
/* retourne 0 si ok sinn 1 */

concept(Lequiv,Linst,LinstR,Result):-
                setof(X, cnamea(X),Lcnamea),setof(X, cnamena(X),Lcnamena),concat(Lcnamena,Lcnamea,Lconcept)
                ,setof(X, iname(X),Liname),setof(X, rname(X),Lrname)
                ,verife_inst(Lconcept,Liname,Lrname,Linst,0,Result1)
                ,verife_instR(Liname,Lrname,LinstR,Result1,Result2)
               ,verife_equiv(Lrname,Lconcept,Lcnamena,Lequiv,Result2,Result),!.

verife_inst(_,_,_,[],R,R):-!.
verife_inst(Lconcept,Liname,Lrname,[(X,C)|Linst],Init,Result):-
                        recupere_concept_role(C,List_concept,List_role),
                        appertient_2(X,Liname,Init,Result1),
                        appertient_list(List_concept,Lconcept,Result1,Result2),
                        appertient_list(List_role,Lrname,Result2,Result3)
                        ,verife_inst(Lconcept,Liname,Lrname,Linst,Result3,Result),!.

verife_instR(_,_,[],Result1,Result1):-!.
verife_instR(Liname,Lrname,[(X,Y,Z)|LinstR],Init,Result):-appertient_2(X,Liname,Init,Result1),
                                                         appertient_2(Y,Liname,Result1,Result2),
                                                         appertient_2(Z,Lrname,Result2,Result3)
                                                        ,verife_instR(Liname,Lrname,LinstR,Result3,Result),!.

verife_equiv(_,_,_,[],Result2,Result2):-!.
verife_equiv(Lrname,Lconcept,Lcnamena,[(CC,C)|Lequiv],Init,Result):- 
        recupere_concept_role(C,List_concept,List_role) ,appertient_2(CC,Lcnamena,Init,Result1),
        appertient_list(List_concept,Lconcept,Result1,Result2),appertient_list(List_role,Lrname,Result2,Result3)
        ,verife_equiv(Lrname,Lconcept,Lcnamena,Lequiv,Result3,Result),!.

/* remarque 3 */
/* principe parcours par pronfendur */
/*retourne une liste en nnf concept simple*/

traitement_Tbox(Lequiv,Result):- t_b(Lequiv,[],Result).

t_b([],Init,Init).
t_b([(C,X)|List],Init,Result):-
                 suit_T_B(X,Result1),nnf(Result1,Result2),concat(Init,[(C,Result2)],New_List),t_b(List,New_List,Result). 

suit_T_B(or(C1,C2),or(Result1,Result2)):-
                 suit_T_B(C1,Result1), 
                 suit_T_B(C2,Result2),! .
suit_T_B(and(C1,C2),and(Result1,Result2)):- 
                 suit_T_B(C1,Result1), 
                 suit_T_B(C2,Result2),! .
suit_T_B(some(R,C),some(R,Result1)):- 
                suit_T_B(C,Result1),!.
suit_T_B(all(R,C),all(R,Result1)):-          
                suit_T_B(C,Result1),!.
suit_T_B(not(or(C1,C2)),not(or(Result1,Result2))):-
                 suit_T_B(C1,Result1), 
                 suit_T_B(C2,Result2),! .
suit_T_B(not(and(C1,C2)),not(and(Result1,Result2))):-
                 suit_T_B(C1,Result1), 
                 suit_T_B(C2,Result2),! .
suit_T_B(not(some(R,C)),not(some(R,Result1))):- 
                suit_T_B(C,Result1),!.
suit_T_B(not(all(R,C)),not(all(R,Result1))):- 
                suit_T_B(C,Result1),!.
suit_T_B(not(C),not(Result1)):- 
                suit_T_B(C,Result1),!.
suit_T_B(C,Result):- 
                cnamena(C),equiv(C,X),suit_T_B(X,Result),!.
suit_T_B(C,C):- cnamea(C),!.

nnf(not(and(C1,C2)),or(NC1,NC2)):- nnf(not(C1),NC1), nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)):- nnf(not(C1),NC1), nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)):- nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)):- nnf(not(C),NC),!.
nnf(not(not(X)),NC):- nnf(X,NC) ,!.
nnf(not(anything),nothing):-!.
nnf(not(nothing),anything):-!.
nnf(not(X),not(X)):-!.
nnf(and(C1,C2),and(NC1,NC2)):- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)):- nnf(C1,NC1), nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)):- nnf(C,NC),!.
nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).

/* a:c c complexe -> que des atomique */
/* remaruqe 4 */

traitement_Abox(Tbox,Linst,Abox_inst):-
        traitement_Tbox(Tbox,New_tbox),simple_Abox_inst(New_tbox,Linst,[],Abox_inst),!.

simple_Abox_inst(_,[],Init,Init):-!.

simple_Abox_inst(Tbox,[(X,C)|List],Init,Result):- suit_4(Tbox,C,New_c),nnf(New_c,Nnf_c),
                                                concat(Init,[(X,Nnf_c)],New_List),
                                                simple_Abox_inst(Tbox,List,New_List,Result),!.

suit_4(Tbox,or(C1,C2),or(New_C1,New_C2)):- suit_4(Tbox,C1,New_C1), suit_4(Tbox,C2,New_C2), !.
suit_4(Tbox,and(C1,C2),and(New_C1,New_C2)):- suit_4(Tbox,C1,New_C1), suit_4(Tbox,C2,New_C2), !.
suit_4(Tbox,some(R,C),some(R,New_C)):- suit_4(Tbox,C,New_C), !.
suit_4(Tbox,all(R,C),all(R,New_C)):- suit_4(Tbox,C,New_C), !.
suit_4(Tbox,not(or(C1,C2)),not(or(New_C1,New_C2))):- suit_4(Tbox,C1,New_C1), suit_4(Tbox,C2,New_C2), !.
suit_4(Tbox,not(and(C1,C2)),not(and(New_C1,New_C2))):- suit_4(Tbox,C1,New_C1), suit_4(Tbox,C2,New_C2), !.
suit_4(Tbox,not(some(R,C)),not(some(R,New_C))):- suit_4(Tbox,C,New_C), !.
suit_4(Tbox,not(all(R,C)),not(all(R,New_C))):- suit_4(Tbox,C,New_C), !.
suit_4(Tbox,not(not(C)),not(not(New_C))):- suit_4(Tbox,C,New_C), !.
suit_4(Tbox,not(C),not(New_C)):- suit_4(Tbox,C,New_C), !.
suit_4(_,C,C):- cnamea(C), !.
suit_4(Tbox,C,New_C):- cnamena(C),get_second(Tbox,C,New_C), !.

/*






                PARTIE 2










*/

saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :- 
        nl,write('Entrez le numero du type de proposition que vous voulez demontrer :'),nl, 
        write('1 Une instance donnee appartient a un concept donne.'),nl,
        write('2 Deux concepts nont pas delements en commun.(ils ont une intersection vide)'),nl,
        read(R), suite(R,Abi,Abi1,Tbox).
suite(1,Abi,Abi1,Tbox) :- acquisition_prop_type1(Abi,Abi1,Tbox),!.
suite(2,Abi,Abi1,Tbox) :- acquisition_prop_type2(Abi,Abi1,Tbox),!.
suite(_,Abi,Abi1,Tbox) :- nl,write('Cette reponse est incorrecte.'),nl, saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

acquisition_prop_type1(Abi,Abi1,Tbox):- 
                nl,write('Entrez inst(I,C)'),nl,
                read(Prop),remplacement_concept_1([Abi,Abi1,Tbox],Prop,New_prop),concat(Abi,[New_prop],Abi1),!.

remplacement_concept_1([Abi,Abi1,Tbox],inst(I,C),_):- 
        setof(X, iname(X),Liname), setof(X, rname(X),Lrname),
        setof(X, cnamea(X),Lcnamea),setof(X, cnamena(X),Lcnamena),concat(Lcnamena,Lcnamea,Lconcept),
        verife_inst(Lconcept,Liname,Lrname,[(I,C)],0,1),write('vous devez entrez des truc existant deja\n'),
        saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox),!.
remplacement_concept_1([_,_,Tbox],inst(I,C),(I,Nnf_c)):- 
                traitement_Tbox(Tbox,New_tbox), suit_4(New_tbox,C,New_c),nnf(not(New_c),Nnf_c),!.
remplacement_concept_1([Abi,Abi1,Tbox],_,_):- 
                write('vous devez entrez un type instance\n'),saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox),!.

acquisition_prop_type2(Abi,Abi1,Tbox):- 
                nl,write('Entrez AND(C1,C2)'),nl,
                read(Prop),verife_type_2([Abi,Abi1,Tbox],Prop,New_prop),concat(Abi,[New_prop],Abi1),!.

verife_type_2([Abi,Abi1,Tbox],and(C1,C2),_):-
                recupere_concept_role(and(C1,C2),List_concept,List_role),setof(X, cnamea(X),Lcnamea),
                setof(X, cnamena(X),Lcnamena),concat(Lcnamena,Lcnamea,Lconcept),setof(X, rname(X),Lrname), 
                appertient_list(List_concept,Lconcept,0,Result1),appertient_list(List_role,Lrname,Result1,1),
                write('vous devez utilisé que des truc definie deja\n'),
                saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox),!.

verife_type_2([_,_,Tbox],and(C1,C2),(Inst,Nnf_c)):- 
                traitement_Tbox(Tbox,New_tbox), suit_4(New_tbox,and(C1,C2),New_c), nnf(New_c,Nnf_c) ,write('1\n'), genere(Inst),!.
verife_type_2([Abi,Abi1,Tbox],_,_):- write('vous devez entrez un type and\n'),saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox),!.

/*






                PARTIE 3










*/

/* resolution avec some */
resolution(Lie,Lpt,Li,Lu,Ls,Abr):- 
                                nl,write('Racine de resolution'),nl,
                                complete_some(Lie,Lpt,Li,Lu,Ls,Abr) ,!.

/* si some non fait on passe au and */
complete_some([],Lpt,Li,Lu,Ls,Abr) :- transformation_and([],Lpt,Li,Lu,Ls,Abr) ,!.
complete_some([(I,some(R,C))|Lie],Lpt,Li,Lu,Ls,Abr) :-
                                nl,write('Evolution avec la règle some'),nl,
                                genere(B),
                                evolue((B,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
                                affiche_evolution_Abox(Ls, [(I,some(R,C))|Lie], Lpt, Li, Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, [(I,B,R)|Abr]),
                                read(_),
                                test_clash(Lie1, Lpt1, Li1, Lu1, Ls1, [(I,B,R)|Abr]) ,!.

/* si and non fait on passe au all */
transformation_and([],Lpt,[],Lu,Ls,Abr) :- deduction_all([],Lpt,[],Lu,Ls,Abr) ,!.
transformation_and(Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls,Abr) :-
                                nl,write('Evolution avec la règle and'),nl,
                                evolue((I,C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
                                evolue((I,C2), Lie1, Lpt1, Li1, Lu1, Ls1, Lie2, Lpt2, Li2, Lu2, Ls2),
                                affiche_evolution_Abox(Ls, Lie, Lpt, [(I,and(C1,C2))|Li], Lu, Abr, Ls2, Lie2, Lpt2, Li2, Lu2, Abr),
                                read(_),
                                test_clash(Lie2, Lpt2, Li2, Lu2, Ls2, Abr) ,!.

/* si all non fait on passe au or */
deduction_all([],[],[],Lu,Ls,Abr) :- transformation_or([],[],[],Lu,Ls,Abr), !.
deduction_all([],[(I,all(R,C))|Lpt],[],Lu,Ls,Abr) :-
                                nl,write('Evolution avec la règle all'),nl,
                                evolue_all((I,all(R,C)),Abr, [], Lpt, [], Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
                                nl,write('ALLL'),nl,
                                affiche_evolution_Abox(Ls, [], [(I,all(R,C))|Lpt], [], Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr),
                                read(_),
                                test_clash(Lie1, Lpt1, Li1, Lu1, Ls1, Abr) ,!.
evolue_all(_, [], Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :- nl,write('ici 1'),nl,!.
evolue_all((I,all(R,C)), [(I,B,R)|Abr], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
                                nl,write('ici 2'),print(Abr),nl,
                                evolue((B,C),Lie, Lpt, Li, Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
                                evolue_all((I,all(R,C)), Abr, Lie2, Lpt2, Li2, Lu2, Ls2, Lie1, Lpt1, Li1, Lu1, Ls1), !.
evolue_all((I,all(R,C)), [R1|Abr], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
                                nl,write('ici 3'),print(Abr),nl,
                                evolue_all((I,all(R,C)), Abr, Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1), !.

/*
evolue_all((michelAnge,all(aEcrit,not(livre))),[(michelAnge,david,aCree),(michelAnge,sonnets,aEcrit),(vinci,joconde,aCree)],
        [],[],[],[],[(david,sculpture),(joconde,objet),(michelAnge,personne),(sonnets,livre),(vinci,personne)],
        Lie1, Lpt1, Li1, Lu1, Ls1).
evolue_all((michelAnge, all(aEcrit, not(livre))), [(vinci, joconde, aCree)],
        [], [], [], [], [(sonnets, not(livre)),  (david, sculpture),  (joconde, objet),  (michelAnge, personne),  (sonnets, livre),  (vinci, personne)],
        Lie1, Lpt1, Li1, Lu1, Ls1).
*/

/* plus aucune règle à appliquer */
transformation_or([],[],[],[],Ls,_) :- 
                                nl,write('Clash dans:'),print(Ls),
                                clash(Ls,1), !.
transformation_or([],[],[],[(I,or(C1,C2))|Lu],Ls,Abr) :-
                                nl,write('Evolution avec la règle or branche 1'),nl,
                                evolue((I,C1), [], [], [], Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
                                affiche_evolution_Abox(Ls, [], [], [], [(I,or(C1,C2))|Lu], Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr),
                                read(_),
                                test_clash(Lie1, Lpt1, Li1, Lu1, Ls1, Abr) ,
                                
                                nl,write('Evolution avec la règle or branche 2'),nl,
                                evolue((I,C2), [], [], [], Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
                                affiche_evolution_Abox(Ls, [], [], [], [(I,or(C1,C2))|Lu], Abr, Ls2, Lie2, Lpt2, Li2, Lu2, Abr),
                                read(_),
                                test_clash(Lie2, Lpt2, Li2, Lu2, Ls2, Abr) , !.

affiche_evolution_Abox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1, Ls2, Lie2, Lpt2, Li2, Lu2, Abr2) :-
                                nl,nl,write('Abox Avant:'),
                                nl,write('    Ls:  '), print(Ls1),
                                nl,write('    Lie: '), print(Lie1),
                                nl,write('    Lpt: '), print(Lpt1),
                                nl,write('    Li:  '), print(Li1),
                                nl,write('    Lu:  '), print(Lu1),
                                nl,write('    Abr: '), print(Abr1),
                                nl,write('Abox Après:'),
                                nl,write('    Ls:  '), print(Ls2),
                                nl,write('    Lie: '), print(Lie2),
                                nl,write('    Lpt: '), print(Lpt2),
                                nl,write('    Li:  '), print(Li2),
                                nl,write('    Lu:  '), print(Lu2),
                                nl,write('    Abr: '), print(Abr2), !.

tri_Abox([],[],[],[],[],[]) :- !.
tri_Abox([(I,some(R,C))|Abi],[(I,some(R,C)) | Lie],Lpt,Li,Lu,Ls) :-
                                                tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls) ,!.
tri_Abox([(I,all(R,C))|Abi],Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls) :-
                                                tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls) ,!.
tri_Abox([(I,and(C1,C2))|Abi],Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls) :-
                                                tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
tri_Abox([(I,or(C1,C2))|Abi],Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls) :-
                                                tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
tri_Abox([As_res|Abi],Lie,Lpt,Li,Lu,[As_res|Ls]) :-
                                                tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.

evolue((I,some(R,C)), Lie, Lpt, Li, Lu, Ls, [(I,some(R,C)) | Lie], Lpt, Li, Lu, Ls) :- !.
evolue((I,all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, [(I,all(R,C))|Lpt], Li, Lu, Ls) :- !.
evolue((I,and(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, [(I,and(C1,C2))|Li], Lu, Ls) :- !.
evolue((I,or(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, [(I,or(C1,C2))|Lu], Ls) :- !.
evolue(As_res, Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [As_res|Ls]) :- !.

/* liste vide */
clash([],0) :- !.
/* un seul element (I,C) ou (I,not(C)) s'unifient */
clash([(_,_)],0) :- !.
clash([(I,C)|Y],1) :- nnf(not(C),Nnf_c), appertient((I,Nnf_c),Y,1)  ,!.
clash([(_,_)|Y],Result) :- clash(Y,Result)  ,!.

test_clash(_, _, _, _, Ls, _) :- clash(Ls,1), !.
test_clash(Lie, Lpt, Li, Lu, Ls, Abr) :- resolution(Lie, Lpt, Li, Lu, Ls, Abr), !.

genere(Nom) :-  compteur(V),nombre(V,L1),
                concat([105,110,115,116],L1,L2),
                V1 is V+1,
                dynamic(compteur/1),
                retract(compteur(V)),
                dynamic(compteur/1),
                assert(compteur(V1)),nl,nl,nl,
                name(Nom,L2).
nombre(0,[]).
nombre(X,L1) :-
                R is (X mod 10),
                Q is ((X-R)//10),
                chiffre_car(R,R1),
                char_code(R1,R2),
                nombre(Q,L),
                concat(L,[R2],L1).

chiffre_car(0,'0').
chiffre_car(1,'1').
chiffre_car(2,'2').
chiffre_car(3,'3').
chiffre_car(4,'4').
chiffre_car(5,'5').
chiffre_car(6,'6').
chiffre_car(7,'7').
chiffre_car(8,'8').
chiffre_car(9,'9').

/*






                FONCTIONS qui recupere les role et les concept utilisé










*/

recupere_concept_role(or(C1,C2),List_concept,List_role):- recupere_concept_role(C1,List_concept1,List_role1),
                recupere_concept_role(C2,List_concept2,List_role2),concat(List_concept1,List_concept2,List_concept),
                concat(List_role1,List_role2,List_role), ! .
recupere_concept_role(and(C1,C2),List_concept,List_role):- recupere_concept_role(C1,List_concept1,List_role1),
                recupere_concept_role(C2,List_concept2,List_role2),concat(List_concept1,List_concept2,List_concept),
                concat(List_role1,List_role2,List_role), ! .
recupere_concept_role(all(R,C),List_concept,List_role) :- recupere_concept_role(C,List_concept,List_role1),concat(List_role1,[R],List_role) ,!.
recupere_concept_role(some(R,C),List_concept,List_role) :- recupere_concept_role(C,List_concept,List_role1),concat(List_role1,[R],List_role) ,!.

recupere_concept_role(not(or(C1,C2)),List_concept,List_role):- recupere_concept_role(C1,List_concept1,List_role1),
                recupere_concept_role(C2,List_concept2,List_role2),concat(List_concept1,List_concept2,List_concept),
                concat(List_role1,List_role2,List_role), ! .
recupere_concept_role(not(and(C1,C2)),List_concept,List_role):- recupere_concept_role(C1,List_concept1,List_role1),
                recupere_concept_role(C2,List_concept2,List_role2),concat(List_concept1,List_concept2,List_concept),
                concat(List_role1,List_role2,List_role), ! .
recupere_concept_role(not(all(R,C)),List_concept,List_role) :- recupere_concept_role(C,List_concept,List_role1),
                concat(List_role1,[R],List_role) ,!.
recupere_concept_role(not(some(R,C)),List_concept,List_role) :- recupere_concept_role(C,List_concept,List_role1)
                ,concat(List_role1,[R],List_role) ,!.
recupere_concept_role(not(C),List_concept,List_role):- recupere_concept_role(C,List_concept,List_role), ! .
recupere_concept_role(C,[C],[]):-!.
recupere_concept_role(C,[C],[]):-!.

/*






                FONCTIONS AUXILIERES










*/

concat([],L1,L1).
concat([X|Y],L1,[X|L2]) :- concat(Y,L1,L2).

appertient(C,[C|_],1).
appertient(_,[],0).
appertient(C,[Y|L],Result) :- Y\==C,appertient(C,L,Result).

appertient_list(_,_,1,1).
appertient_list([],_,Result,Result).
appertient_list([X|List],L,Init,Result):-appertient_2(X,L,Init,Result1),appertient_list(List,L,Result1,Result).

appertient_2(_,_,1,1).
appertient_2(_,[],_,1).
appertient_2(X,[X|_],V,V).
appertient_2(X,[Y|L],V,V_r):-X\==Y, appertient_2(X,L,V,V_r).

get_second([(Y,X)|_],Y,X).
get_second([(C,_)|Tbox],Y,New_Y):- C\==Y , get_second(Tbox,Y,New_Y).

stop_program(L):- appertient_2(1,L,0,0),halt. 
stop_program(_).
