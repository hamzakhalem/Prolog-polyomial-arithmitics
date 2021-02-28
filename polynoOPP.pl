/************************** printing ************************/
affichage(0,_):-write(''),!. /* case zero*/
affichage(X,0):-write(X),!. /*case constante*/
affichage(1,1):- write(' + x '),!.
affichage(-1,1):- write(' - x '),!.
affichage(X,1):- X>0,write(' + '), write(X),write('x'),!. /*pour puissnace 1 positive*/
affichage(X,1):- X<0, write(X),write('x'),!. /*pour puissnace 1 négative*/
affichage(1,P):- write(' + '),write('x^'),write(P),!.
affichage(-1,P):- write(' - '),write('x^'),write(P),!.
affichage(X,P):- X>0,write(' + '), write(X),write('x^'),write(P),!.

/*pour X positive*/
affichage(X,P):- X<0, write(X),write('x^'),write(P).
/*pour x nÃ©gative*/
parcourireAffiche([[X,M]|[]]):-affichage(X,M),!.
/*pour une seul item dans la liste*/
parcourireAffiche([[X,M]|L]):-is_list(L),affichage(X,M),parcourireAffiche(L),!.
/*plsieur item dans la liste parcoure recursive*/
/******* test : parcourireAffiche([[-1,4],[+1,6],[0,7],[4.988,78],[1,1],[-1,-3]]). ******/
/************************** la licture ************************/
test(L):-write('vouler vous ajouter ? oui / non') ,read(K),K==oui,lireListe(L).
test([]).
lireListe(L):-write('a='),read(A),nl,write('p='),read(P),test(T),append([[A,P]],T,L).
/****** test: lireListe(L). ******/
/**************************** evalouation d'un polynome *************************/
puiss(-,0,1). /*pour zero c'est 1)*/
puiss(T,1,T):-!. /*pour 1 */
puiss(T,N,R):-N1 is N-1,puiss(T,N1,R1),R is T*R1. /*recursvité*/
evalu(_,[],0):-!. /*list vide*/
evalu(M,[[_,0]|L],K):-evalu(M,L,K1),K is M+K1. /*puissnace zero */
evalu(M,[[0,_]|L],K):-evalu(M,L,K1),K is K1. /* a=0 */
evalu(M,[[X,P]|[]],K):-puiss(M,P,R),K is X*R,!. /*item final */
evalu(M,[[X,P]|L],K):-puiss(M,P,R),evalu(M,L,K1),T is R*X,K is K1+T. /*cas génral*/
/********** test: evalu(1,[[-3,1],[0,9],[0.4,1],[+16,1]],L). resultat : L = 13.399999999999999 ******/
/**********************************la dérivation ******************/
drive([],[]):-!.
drive([[_,0]|L],[[0,0]|S]):-drive(L,S),!.
drive([[X,P]|L],[[T,R]|S]):- T is X*P,R is P-1,drive(L,S).
/***********  test: drive([[4,8],[0,7],[-3,8],[2,3]],L). resultat : L = [[32, 7], [0, 6], [-24, 7], [6, 2]]. *******/
/********************* simplification *****************************/
/******* superision de puisance ****/
suppPuiss([[X,P1]],P,[[X,P1]]):-P=\=P1.
suppPuiss([[_,P]],P,[]):-!.
suppPuiss([[_,P]|L],P,R):-suppPuiss(L,P,R),!.
suppPuiss([[X,P1]|L],P,[[X,P1]|R]):-P=\=P1,suppPuiss(L,P,R).
/******* canter les  ai de puisance ****/
ajouterPuiss([],_,0):-!.
ajouterPuiss([[_,R]|[]],P,0):- R=\=P,!.
ajouterPuiss([[X,P]|[]],P,X):-!.
ajouterPuiss([[X,P]|L],P,M):-ajouterPuiss(L,P,S),M is S+X.
ajouterPuiss([[_,P1]|L],P,M):-P1=\=P,ajouterPuiss(L,P,S),M is S.
smplifier([],[]). /*cas vide */
smplifier([[X,P]],[[X,P]]). /*cas final*/
smplifier([[X,P]|L],[[M,P]|R]):-ajouterPuiss([[X,P]|L],P,M),suppPuiss(L,P,T),smplifier(T,R).
/************ smplifier([[4,7],[-3,7],[6,3],[2,2],[4,2]],L). resultat : L = [[1, 7], [6, 3], [6, 2]] **********/
/************************ La somme ********************************/
somme(L,P,M):-append(L,P,V),smplifier(V,M). /*cancatner les deuc list apres les symplfier */
/********************** test: somme([[4,7],[-3,7],[6,3],[2,2],[4,2]],[[4,7],[-6,4],[6,4],[-2,7],[6,3]],P). resultat : P = [[3, 7], [12, 3], [6, 2], [0, 4]] ******/
/***************** la sustraction ******************/
negPoly([],[]):-!.
negPoly([[X,P]],[[X1,P]]):-X1 is - X,!.
negPoly([[X,P]|L],[[X1,P]|S]):-X1 is - X,negPoly(L,S).
sustraction(L,P,M):-negPoly(P,S),somme(L,S,M). /*négative de deuxiem liste est la somme avec la premier */
/******* test: sustraction([[4,7],[-3,7],[6,3],[2,2],[4,2]],[[4,7],[-6,4],[5,4],[-2,7],[6,3]],P). resultat : P = [[-1, 7], [0, 3], [6, 2], [1, 4]] ********/
/*********************** le produit **************/
monmproduit([_,_],[],[]):-!.
monmproduit([X1,P1],[[X2,P2]|L],[[X,P]|R]):- X is X1*X2,P is P1+P2,monmproduit([X1,P1],L,R).
produit([],_,[]):-!.
produit([[X,P]|L],R,S):-monmproduit([X,P],R,M),produit(L,R,V),append(M,V,S). /*prdouit de chaque monm avec l'autre polynome*/
/********* test: produit([[-1,-2],[0,3]],[[3,4],[5,6]],S). resultat : S = [[-3, 2], [-5, 4], [0, 7], [0, 9]].********/
/*********************************** part deux *******************/
:-op(700,xfy,'est').
:-op(300,xfy,'+').
:-op(600,fy,'simp').
:-op(600,fy,'driv').
:-op(500,xfy,'-').
:-op(200,xfy,'*').
nv1(simp K,V):-nv1(K,T1),smplifier(T1,V).
nv1(driv K,V):-nv1(K,T1),drive(T1,V).
nv1(X,V):-nv2(X,V).
nv2(X - Y ,V ):-nv2(Y,T1),nv3(X,T2),sustraction(T2,T1,V).
nv2(X,V):-nv3(X,V).
nv3(X + Y,V):-nv3(Y,T),nv4(X,T2),somme(T2,T,V).
nv3(X,V):-nv4(X,V).
nv4(X * Y,V ):-nv4(Y,T),produit(X,T,V).
nv4(X,X).
X est Y :-nv1(Y,Z),X = Z .
/******* test total :  P est simp driv [[3,7],[4,8]] + [[3,8]] - [[1,2]] * [[4,6]]. P = [[21, 6], [24, 7]] *******/
