(*Définition de type *)

type semaine = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche;;
let estwe (j : semaine ): bool = 
  j = Samedi || j= Dimanche;;


type figures =
  | Carre of float
  | Rectangle of float*float
  | Cercle of float ;;

let aire = function
  | Carre c -> airecarre c
  | Rectangle (a,b) -> airerectangle a b
  | Cercle c -> airecercle c;;


type complexe = { re : float; im : float };;

let (c : complexe ) = { re = 3. ; im = 5.};;


(* Liste *)

let maListe = 2::5::-5::[];;

let rec longueur l = match l with
  | [] -> 0
  | x::reste -> 1 + (longueur reste);;
  
let rec somme l = match l with
  | [] -> 0
  | x::reste -> x + (somme reste);;

let rec renverse ( l : 'a list)(res : 'a list) : 'a list =
  match l with
  | [] -> res
  | e::f -> (renverse f (e::res));;


(* Liste et type *)

type categorie  =  LecteurMP3 | AppareilPhoto | Camera| Telephone | Ordinateur;;

type marque = Alpel | Syno | Liphisp;;

type appareil = categorie*marque*int*int;;

let (appareilA:appareil) = (LecteurMP3,Alpel,100,5);;
let (appareilB:appareil) = (AppareilPhoto,Syno,150,3);;
let (appareilC:appareil) = (Telephone,Alpel,1000,50);;    
let (appareilD:appareil) = (Ordinateur,Liphisp,500,15);;
let (appareilE:appareil) = (Ordinateur,Alpel,1500,0);;
  
let monMagasin= appareilA::appareilB::appareilC::appareilC::[];;

(* Utilisation de when pour avoir un motif de filtrage lié au paramêtre *)
let rec estEnStock ( l : appareil list )( c : categorie ) ( m : marque )( p : int) : bool =
  match l with
  | [] -> false
  | (c1,m1,p1,s)::reste when c1=c && m1=m && p1=p -> s>0
  | _::reste -> estEnStock reste c m p;;


(* Génération de nombre aléatoire *)

let rec aleatoire (l : int) : int list =
  let valeur_max = 2*l in
  match l with
  | 0 -> []
  | x -> (Random.int(valeur_max))::(aleatoire (l-1));;


(* Tris *)

let rec trouve_min ( l : 'a list) : 'a*'a list =
  match l with
  | [] -> failwith "liste non dÃ©finie"
  | [e] -> (e,[])
  | e1::e2::f -> let (m,k)= (trouve_min (e2::f)) in
		 if e1<m then
		   (e1,e2::f)
		 else
		   (m,e1::k);;


let rec tri_selection ( l: 'a list) : 'a list =
  match l with
  | [] -> []
  | _ -> let (m,k) = (trouve_min l) in m ::(tri_selection k);;

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


let rec insere ( l : 'a list)( element : 'a) : 'a list =
  match l with
  | [] -> element::[]
  | t::res -> if t<element then
		t::(insere res element)
	      else
		element::t::res;;


let rec tri_insertion ( l : 'a list) : 'a list =
  match l with
  | [] -> []
  | t::res -> insere (tri_insertion res) t;;


let rec decoupe ( pivot : 'a)( l : 'a list ) : 'a list * 'a list =
  match l with
  | [] -> ([],[])
  | e::suite -> let (a,b) = (decoupe pivot suite) in
	      if pivot > e then
		(e::a, b)
	      else
		(a,e::b);;

		   
let rec tri_rapide (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | e::fin -> let (a,b) = (decoupe e fin) in
	      (tri_rapide a) @ [e] @ (tri_rapide b);;


let rec fusion (l1 : 'a list) (l2 : 'a list) : 'a list =
  match (l1,l2) with
  | ([],[]) -> []
  | ([],reste2) -> reste2
  | (reste1,[]) -> reste1
  | ((t1::reste1), (t2::reste2)) -> if t1>t2 then
			       t2::(fusion (t1::reste1) reste2)
			     else
			       t1::(fusion reste1 (t2::reste2));;


let rec coupe (l : 'a list) : 'a list * 'a list =
  match l with
  | [] -> ([],[])
  | [e] -> ([e],[])
  | t1::t2::reste -> let (d,g) = (coupe reste) in
		     ((t1::d),(t2::g));;

    
let rec tri_fusion ( l : 'a list) : 'a list =
  match l with
  | [] -> []
  | [e] -> [e]
  | l -> let (m,n) = (coupe l) in
	 fusion (tri_fusion m) (tri_fusion n);;



(* Utilisation de fonctions du système *)
open Printf;;
read_int;;

printf "Mon texte";;

let nombreElementListe = read_int();;
let tps0 = Sys.time();;

(* File *)

let rac ( l : 'a list ) : 'a * 'a list =
  match l with
  | [] -> failwith "Liste vide"
  | x -> let renversee = (renverse l []) in
	 match renversee with
	 | (t::reste) ->(t, (renverse reste []));;

 
type file = int list;;

let (file_vide : file ) = [];;

let est_file_vide (file : file) : bool =
  match file with
  | [] -> true
  | _ -> false;;

let enfile (element : int)(file : file) : file =
  element::file;;
  
let defile (file : file) : int * file =
  rac file;;


(* Array *)
Array.length;;
Array.get;;    
Array.make;;
Array.set;;    

let get (t : int array array) ( x : int ) ( y : int) : int =
  let colonne = (Array.get t x) in
  Array.get colonne y ;;


let set (t : int array array) ( x : int ) ( y : int) ( v : int ): unit =
  let colonne = (Array.get t x) in
  Array.set colonne y x ;;





