Mottet Loic & Geourjon Anthony

APF - Compte rendu TP1

**********************************

Exercice 1:
# val r : int = 42
# val a : int = 0
Erreur, x n'est pas défini.
# val u : int = 2
# val l : int = 35

Exercice 2:

let f1 =  fun x -> fun y -> fun z ->  x*y*z;;
let f2=  fun x -> fun y -> fun z ->  x+y+z;;

Exercice 3:

-let somme = fun x -> fun y -> x+y;;
-let sup = fun (x:int) -> fun y -> x>y;;
-f3 est une fonction à deux parametres dont le premier est une fonction qui prend un int en parametre et renvoie un boolée,. le second est un entier et la fonction retourne un bool. Le type de f3 est :
val f3 : (int -> bool) -> int -> bool = <fun>
-f4 est une fonction à deux paramretres dont le premier est une fonction qui prend un int en parametre et renvoie un int, le second est un entier et la fonction retourne un int. Le type de f4 est :
val f4 : (int -> int) -> int -> int = <fun>
-f5 est une fonction qui prend un parametre et qui retourne deux valeurs de ce même parametre. C'est évidement impossible et cela provoque une erreur.

Excercice 4: 

lef f = fun i -> fun x -> x + i;;
let g = f 2;;

let f1 = fun a -> fun b -> b*a;;
let g1 = f1 a;; 

A REPRENDRE !!

Exercice 5 :

-let positif = fun x -> x>=0;;
Appel : positif (-2);;
-let paire= fun x -> (x mod 2)=0;;
Appel : paire 2;;
-let triplet a b c = (a**2)=(b**2+c**2);; où b^2 + c^2 = a^2
Appel 5 3 4 -> vrai
-let memeSigne a b = (positif a)=(positif b);;
Appel : memeSigne (-2) 3

Exercice 6 :

let min2entiers (a : int ) (b : int ): int =
	if (a<b) then a 
	else b;;
Appel : min2entiers 4 2 ;;

Exercice 7 :

let max2entiers ( a : int ) (b : int ):int =
	if (a>b) then a else b;;
Appel : max2entiers 3 66;;

Exercice 8 :

let min3entiers ( a : int) (b : int) (c : int): int =
(min2entiers ( min2entiers a b) c) ;;
Appel : min3entiers 3 5 1

Exercice 9 :

type semaine = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche;;
let estwe (j : semaine ): bool = 
  j = Samedi || j= Dimanche;;

Appel : estwe Samedi;;

Exercice 10 :

let airecarre ( a: float ) : float =
a*.a;;
airecarre 5.;;

let airerectangle ( a : float ) ( b :  float) : float=
a*.b;;
airerectangle 3. 4. ;;

let airecercle ( r : float ): float =
3.14*.r**2.;;

airecercle 4.;;

let airetrirect( a: float ) ( h : float ) : float =
  let b = sqrt(h*.h -. a*.a) in a*.b/.2.;;
airetrirect 5. 6.;;

Exercice 11:

let airecarre ( a: float ) : float =a*.a;;

let airerectangle ( a : float ) ( b :  float) : float=a*.b;;

let airecercle ( r : float ): float = 3.14*.r**2.;;

type figures =
  | Carre of float
  | Rectangle of float*float
  | Cercle of float ;;

let aire = function
  | Carre c -> airecarre c
  | Rectangle (a,b) -> airerectangle a b
  | Cercle c -> airecercle c;;

Exercice 12:

type complexe = { re : float; im : float };;
let (c : complexe ) = { re = 3. ; im = 5.};;
let (d : complexe ) = { re = 6. ; im =7.};;

let (elemneutre : complexe) = {re = 0.; im = 0.};;
let additioncomp ( c :complexe )(e : complexe) : complexe =
  let (d : complexe ) = { re = c.re +. e.re ; im = c.im +. e.im} in d;;
    
additioncomp c d;;

let modulecomp (c : complexe ) : float =
 sqrt (c.re*.c.re+.c.im*.c.im);;

modulecomp(c);;

let oppose ( c: complexe ) : complexe =
{re= -.c.re ; im = -.c.im };;

oppose c;;

Exercice 13 :

type point = { x : float; y : float };;

let distance (a: point) (b: point): float =
    sqrt (b.x-.a.x)**2.+.(b.y-.a.y)**2.;;

# let (a : point) = { x=1. ; y= 2.};;
val a : point = {x = 1.; y = 2.}
# let (b : point) = { x=3. ; y= 2.};;
val b : point = {x = 3.; y = 2.}
# distance a b;;
- : float = 2.000000000000000
  
--

type segment = { x : float; y : float };;

let milieu (a: segment) : float =
  (a.y-.a.x)/.2.+.a.x;;

# let (a : segment) = { x=2. ; y= 5.};;
# milieu a ;;

--

type point = { x : float; y : float };;

type vecteur = { x : float; y : float };;

let (a : point) = { x=2. ; y= 3.};;
let (b : point) = { x=2. ; y= 5.};;

let creeVecteur (a:point) (b:point) : vecteur =
  { x=b.x-.a.x ; y=b.y-.a.y };;

--

type point = { x : float; y : float };;
type vecteur = { x : float; y : float };;
let creeVecteur (a:point) (b:point) : vecteur =
  { x=b.x-.a.x ; y=b.y-.a.y };;

type droite = { p : point ; v :vecteur};;

let (d : droite) = { p={ x=1. ; y= 1.} ; v=creeVecteur { x=2. ; y= 5.}  { x=2. ; y= 3.}}

type equaDroite = { a : float ; b : float };;
let equationDeDroite (d: droite) : equaDroite = { a=d.v.y/.d.v.x ; b= ?}

Lorsqu'on a determiné l'équation ax+b de chacune des droites, on obtient un système. En résolvant x et y, on obtient le point d'intersection. Nous n'avons pas su le transcrire en OCaml.

--

Question non traitée.





(*Exercice 16 *)

let maListe = 2::5::-5::[];;

let rec longueur l = match l with
  | [] -> 0
  | x::reste -> 1 + (longueur reste);;
  
let rec somme l = match l with
  | [] -> 0
  | x::reste -> x + (somme reste);;

let estPositif = fun x -> x>=0;;

let rec tous_positifs l = match l with
 | [] -> true
 | x::reste -> (estPositif x) &&  (tous_positifs reste);;

longueur maListe;;
somme maListe;;  
tous_positifs maListe;;
  
(* Exercice 17 *)
  
type contenu =  Meuble | Objet | Cadre | Plante;;

type solidite = Fragile | Robuste;;

type produit = { con : contenu ; sol : solidite ; poids: int };;

type inventaire =
  | Nil
  | Cons of produit*inventaire;;

(* Exercice 18 *)


let produitA = { con=Objet ; sol=Fragile ; poids=1};;
let produitB = { con=Meuble ; sol=Robuste ; poids=100};;
let produitC = { con=Objet ; sol=Robuste ; poids=10};;
let produitD = { con=Plante ; sol=Robuste ; poids=4};;
let produitE = { con=Cadre ; sol=Robuste ; poids=8};;
  
let monInventaire = Cons (produitA, (Cons (produitD, Cons (produitE, Cons(produitC, Cons(produitB, Nil))))));;  

  
let rec fragiles ( l : inventaire ) : int = match l with
  | Nil -> 0
  | Cons(x, reste) -> match x.sol with
		      | Fragile -> 1 + (fragiles reste)
		      | _ -> fragiles reste;;


fragiles monInventaire;;
(* Exercice 19 *)  

let rec legers ( l : inventaire ) ( pmax : int) : inventaire = match l with
  | Nil -> Nil
  | Cons(t,reste) -> if t.poids <= pmax then
		       Cons(t,(legers reste pmax))
		     else
		       legers reste pmax;;

legers monInventaire 7;;  
(* Exercice 20 *)

let rec poids_plante ( l : inventaire )  : int = match l with
  | Nil -> 0
  | Cons (t,reste) -> match t.con with
		      | Plante -> t.poids + (poids_plante reste)
		      | _ -> poids_plante reste;;
						   
poids_plante monInventaire;;

(*Exercice 21 *)

let rec exposition ( l : inventaire )  : inventaire  = match l with
  | Nil -> Nil
  | Cons (t,reste) -> match t.con with
		      | Cadre -> exposition reste
		      | _ -> Cons(t, (exposition reste));;
    
exposition monInventaire;;

(*Exercice 22 *)

let produitF = { con=Objet ; sol=Fragile ; poids=5};;
  
let rec inventorie (l : inventaire) (p : produit) : inventaire = match l with
  | Nil -> Nil
  | Cons (t,reste) -> if p.poids > t.poids then
			Cons(t, (inventorie reste p))
		      else
			Cons(p, Cons(t,reste));;
			    

inventorie monInventaire produitF;;
			    
(*Exercice 23 *)

let rec sub_dromadaire ( l : inventaire ) ( p : produit ) : produit = match l with
  | Nil -> p
  | Cons(t, reste) -> if t.poids > p.poids then
		  sub_dromadaire reste t
		else
		  sub_dromadaire reste p;;
  
(* Ne fonctionne pas avec une liste vide*)  
let dromadaire (l :inventaire) : produit = match l with
  | Cons(t, reste) -> sub_dromadaire l t;;

dromadaire monInventaire;;
(*Exercice 24 Facultatif *)

(*Exercice 25 *)

type categorie  =  LecteurMP3 | AppareilPhoto | Camera| Telephone | Ordinateur;;

type marque = Alpel | Syno | Liphisp;;

type appareil = categorie*marque*int*int;;

let (appareilA:appareil) = (LecteurMP3,Alpel,100,5);;
let (appareilB:appareil) = (AppareilPhoto,Syno,150,3);;
let (appareilC:appareil) = (Telephone,Alpel,1000,50);;    
let (appareilD:appareil) = (Ordinateur,Liphisp,500,15);;
let (appareilE:appareil) = (Ordinateur,Alpel,1500,0);;
  
let monMagasin= appareilA::appareilB::appareilC::appareilC::[];;
  

(*Exercice 26 *)

let rec estEnStock ( l : appareil list )( c : categorie ) ( m : marque )( p : int) : bool =
  match l with
  | [] -> false
  | (c1,m1,p1,s)::reste when c1=c && m1=m && p1=p -> s>0
  | _::reste -> estEnStock reste c m p;;

estEnStock monMagasin LecteurMP3 Alpel 100;;
estEnStock monMagasin LecteurMP3 Apple 100;;

(* Remarque : Attention au match, il crée de nouvelles variables et c,m et p ne sont accessible qu'après avoir été relayés par des variables et le "when". *) 

  
(*Exercice 27*)
  
let rec ajouteArticle ( l : appareil list) ( (c,m,p,s) : appareil ) : appareil list =
  match l with
  | [] -> (c,m,p,s)::[]
  | (c1,m1,p1,s1)::reste when c1=c && m1=m && p1=p  -> (c,m,p,s1+s)::reste
  | _::reste -> ajouteArticle reste (c,m,p,s);;


let (appareilE:appareil) = (Ordinateur,Alpel,1500,2);;

ajouteArticle monMagasin appareilE;;

(*Exercice 28*)

let rec enleve_article (l : appareil list) (a : appareil) : appareil list =
  match l with
  | [] -> []
  | a1::reste when a1=a -> reste
  | x::reste -> x::(enleve_article reste a);;

enleve_article monMagasin appareilA;;
(* Anthony Geourjon & Titouan Larnicol
   Polytech RICM3 Gr.2

   Compte-rendu TP3 OCaml (tri) 

 *)  

(* Exercice 36 *)

let rec est_trie (l : 'a list): bool =
  match l with
  | t1::t2::reste -> if t1 <= t2 then
		       est_trie (t2::reste)
		     else
		       false
  | _ -> true;;


let chaine1 = 'a'::'b'::'c'::[];;
let chaine2 = 'a'::'c'::'b'::[];;
let list1 = 1::2::3::[];;
let list2 = 2::1::3::[];;
  
est_trie chaine1;;
est_trie chaine2;;
est_trie list1;;
est_trie list2;;
est_trie [];;

(*Exercice 37 *)    
    
let rec aleatoire (l : int) : int list =
  let valeur_max = 2*l in
  match l with
  | 0 -> []
  | x -> (Random.int(valeur_max))::(aleatoire (l-1));;

let rec renverse ( l : 'a list)(res : 'a list) : 'a list =
  match l with
  | [] -> res
  | e::f -> (renverse f (e::res));;

let rec decroissante ( longueur : int): 'a list =
  match longueur with
  | 0 -> []
  | x -> x::(decroissante (x-1));;	  

let croissante ( longueur : int) : 'a list =
  (renverse (decroissante longueur ) []) ;;
  

let maListeAleatoire = aleatoire 50;;
let maListeCroissante = croissante 50;;
let maListeDecroissante = decroissante 50;;
  

(*Exercice 38*)

let rec trouve_min ( l : 'a list) : 'a*'a list =
  match l with
  | [] -> failwith "liste non définie"
  | [e] -> (e,[])
  | e1::e2::f -> let (m,k)= (trouve_min (e2::f)) in
		 if e1<m then
		   (e1,e2::f)
		 else
		   (m,e1::k);;

trouve_min maListeAleatoire;;
trouve_min maListeDecroissante;;
trouve_min maListeCroissante;;
(*trouve_min [];;*)
  
(*Exercice 39*)

let rec tri_selection ( l: 'a list) : 'a list =
  match l with
  | [] -> []
  | _ -> let (m,k) = (trouve_min l) in m ::(tri_selection k);;

tri_selection maListeAleatoire;;
tri_selection maListeDecroissante;;
tri_selection maListeCroissante;;
tri_selection [];;

  
(*Exercices 40,41 facultatifs*)

let rec bulles ( l : 'a list) : 'a list * bool =
  match l with
  | [] -> ([],false)
  | [e] -> ([e],false)
  | t1::t2::reste -> if t1>t2 then
		       let (m,n) = (bulles (t1::reste)) in (t2::m,n || true)
		     else
		       let (m,n) = (bulles (t2::reste)) in (t1::m,n || false);;

		       
let rec tri_bulles (l : 'a list) : 'a list =
  let (m,n) = (bulles l) in
  match n with
  | false -> m
  | true -> tri_bulles m;;

tri_bulles maListeAleatoire;;
tri_bulles maListeDecroissante;;
tri_bulles maListeCroissante;;
tri_bulles [];;
  
(*Exercice 42*)

let rec insere ( l : 'a list)( element : 'a) : 'a list =
  match l with
  | [] -> element::[]
  | t::res -> if t<element then
		t::(insere res element)
	      else
		element::t::res;;

(*Exercice 43*)  

let rec tri_insertion ( l : 'a list) : 'a list =
  match l with
  | [] -> []
  | t::res -> insere (tri_insertion res) t;;


tri_insertion maListeAleatoire;;
tri_insertion maListeDecroissante;;
tri_insertion maListeCroissante;;
tri_insertion [];;
    
(*Exercice 44*)

let rec decoupe ( pivot : 'a)( l : 'a list ) : 'a list * 'a list =
  match l with
  | [] -> ([],[])
  | e::suite -> let (a,b) = (decoupe pivot suite) in
	      if pivot > e then
		(e::a, b)
	      else
		(a,e::b);;

(*Exercice 45*)  
		   
let rec tri_rapide (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | e::fin -> let (a,b) = (decoupe e fin) in
	      (tri_rapide a) @ [e] @ (tri_rapide b);;



tri_rapide maListeAleatoire;;
tri_rapide maListeDecroissante;;
tri_rapide maListeCroissante;;
tri_rapide [];;
    


(*Exercices 46-49 facultatifs*)  

let rec fusion (l1 : 'a list) (l2 : 'a list) : 'a list =
  match (l1,l2) with
  | ([],[]) -> []
  | ([],reste2) -> reste2
  | (reste1,[]) -> reste1
  | ((t1::reste1), (t2::reste2)) -> if t1>t2 then
			       t2::(fusion (t1::reste1) reste2)
			     else
			       t1::(fusion reste1 (t2::reste2));;
			       
let a = 2::3::8::15::[];;
let b = 1::5::8::12::20::[];;

fusion a b;;

let rec coupe (l : 'a list) : 'a list * 'a list =
  match l with
  | [] -> ([],[])
  | [e] -> ([e],[])
  | t1::t2::reste -> let (d,g) = (coupe reste) in
		     ((t1::d),(t2::g));;

coupe b;;

    
let rec tri_fusion ( l : 'a list) : 'a list =
  match l with
  | [] -> []
  | [e] -> [e]
  | l -> let (m,n) = (coupe l) in
	 fusion (tri_fusion m) (tri_fusion n);;

tri_fusion maListeAleatoire;;
tri_fusion maListeDecroissante;;
tri_fusion maListeCroissante;;
tri_fusion [];;

(*Exercice 49 (comparaison)

  Manuel :

  Utilisation depuis un terminal : "ocaml cr_tp3_geourjon_larnicol.ml".
  Bug : Lorsque le nombre est trop grand, le script peut provoquer un dépassement de la pile.
  Remarque : Le programme prend environ une centaine de seconde pour un nombre d'éléments équivalent à 10 000. 
*) 


open Printf;;
read_int;;

printf "*********************************************************\n";;
printf "* Comparaison de temps d'éxécution des différents tris. *\n";;
printf "*********************************************************\n\n";;
    
printf "Saisissez la longueur de la liste (un entier >0):\n";;
let nombreElementListe = read_int();;

let tps0 = Sys.time();;
  
let tps1 = Sys.time();;
let liste = aleatoire nombreElementListe;;
let tps2 = Sys.time();;

printf "Génération d'une liste aléatoire de %d éléments (%f s).\n\n" nombreElementListe (tps2-.tps1);;
  
let tps1 = Sys.time();;
tri_selection liste;;
let tps2 = Sys.time();;
tri_bulles liste;;
let tps3 = Sys.time();;
tri_insertion liste;;
let tps4 = Sys.time();;
tri_rapide liste;;
let tps5 = Sys.time();;
tri_fusion liste;;
let tps6 = Sys.time();;

printf "Tri par sélection : %f s pour %d élements.\n" (tps2-.tps1) nombreElementListe;;   
printf "Tri bulles :        %f s pour %d élements.\n" (tps3-.tps2) nombreElementListe;;   
printf "Tri par insertion : %f s pour %d élements.\n" (tps4-.tps3) nombreElementListe;;
printf "Tri rapide :        %f s pour %d élements.\n" (tps5-.tps4) nombreElementListe;;
printf "Tri fusion :        %f s pour %d élements.\n" (tps6-.tps5) nombreElementListe;;

let tps1 = Sys.time();;
let liste = decroissante nombreElementListe;;
let tps2 = Sys.time();;
  
printf "\n\nDéfinition d'une liste decroissante de %d éléments en (%f s).\n\n" nombreElementListe (tps2-.tps1);;

  
let tps1 = Sys.time();;
tri_selection liste;;
let tps2 = Sys.time();;
tri_bulles liste;;
let tps3 = Sys.time();;
tri_insertion liste;;
let tps4 = Sys.time();;
tri_rapide liste;;
let tps5 = Sys.time();;
tri_fusion liste;;
let tps6 = Sys.time();;

printf "Tri par sélection : %f s pour %d élements.\n" (tps2-.tps1) nombreElementListe;;   
printf "Tri bulles :        %f s pour %d élements.\n" (tps3-.tps2) nombreElementListe;;   
printf "Tri par insertion : %f s pour %d élements.\n" (tps4-.tps3) nombreElementListe;;
printf "Tri rapide :        %f s pour %d élements.\n" (tps5-.tps4) nombreElementListe;;
printf "Tri fusion :        %f s pour %d élements.\n" (tps6-.tps5) nombreElementListe;;


let tps1 = Sys.time();;
let liste = croissante nombreElementListe;;
let tps2 = Sys.time();;
  
printf "\n\nDéfinition d'une liste croissante de %d éléments en (%f s).\n\n" nombreElementListe (tps2-.tps1);;

let tps1 = Sys.time();;
tri_selection liste;;
let tps2 = Sys.time();;
tri_bulles liste;;
let tps3 = Sys.time();;
tri_insertion liste;;
let tps4 = Sys.time();;
tri_rapide liste;;
let tps5 = Sys.time();;
tri_fusion liste;;
let tps6 = Sys.time();;

printf "Tri par sélection : %f s pour %d élements.\n" (tps2-.tps1) nombreElementListe;;   
printf "Tri bulles :        %f s pour %d élements.\n" (tps3-.tps2) nombreElementListe;;   
printf "Tri par insertion : %f s pour %d élements.\n" (tps4-.tps3) nombreElementListe;;
printf "Tri rapide :        %f s pour %d élements.\n" (tps5-.tps4) nombreElementListe;;
printf "Tri fusion :        %f s pour %d élements.\n" (tps6-.tps5) nombreElementListe;;         
  
printf "\n\nTemps total d'éxécution du programme %f s.\n" (tps0-.(Sys.time()));;
printf "******************************************************\n";;      
   
