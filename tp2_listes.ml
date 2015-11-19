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
