(* Anthony Geourjon & __
   Polytech RICM3 Gr.2

   Compte-rendu TP4 OCaml (Pile, file, rang) 

 *)

(*Fonctions liées à la création de jeu d'essai*)

let rec aleatoire (l : int) : int list =
  let valeur_max = 2*l in
  match l with
  | 0 -> []
  | x -> (Random.int(valeur_max))::(aleatoire (l-1));;

let maListeAleatoire = aleatoire 50;;  


(* Exercice 50 *)

let rec neme( position : int )(liste: 'a list) : 'a =
  match (position,liste) with
  | (x,[]) -> failwith "La liste ne contient pas la position demandee."
  | (1,t::reste) -> t
  | (x,t::reste) -> neme (x-1) reste;;


let list1 = 1::2::3::4::5::6::[];;

neme 1 list1;;
neme 5 list1;;
neme 8 list1;;

(*Exercice 51 *)
  
let rec renverse ( l : 'a list)(res : 'a list) : 'a list =
  match l with
  | [] -> res
  | e::f -> (renverse f (e::res));;

let rec renverseNombreAppelsRecursif ( l : 'a list)(res : 'a list)( nbAppels : int ) : 'a list * int =
  let nbApp = nbAppels + 1 in 
  match l with
  | [] -> (res,nbApp)
  | e::f -> (renverseNombreAppelsRecursif f (e::res) nbApp);;
  

renverseNombreAppelsRecursif maListeAleatoire [] 1;;
renverseNombreAppelsRecursif list1 [] 1;;

(* Le nombre d'appels récursif de renverse est : nombre d'élement de la liste + 2.
   2 correspond à l'appel initial et à l'appel sur la liste vide qui termine la récurrence. *)
  
(* Exercice 52 *)

type pile = int list;;

let pile_vide = [];;
  
let est_pile_vide ( p : pile) : bool =
  match p with
  | [] -> true
  | _ -> false;;


let depile (pile : pile) : (int * pile) =
  match (pile) with
  | [] -> failwith "Rien a depiler"
  | sommet::reste -> (sommet,reste);;
  
let empile (element : int) (pile : pile) :  pile =
  element::pile;;

let (maPile : pile) = pile_vide;;
let (maPile : pile) = empile 5 maPile;;
let (maPile : pile) = empile 2 maPile;;
let (maPile : pile) = empile 3 maPile;;   
let (element,maPile) = depile maPile;;
let (element,maPile) = depile maPile;;
let (element,maPile) = depile maPile;;
let (element,maPile) = depile maPile;;

(* Exercice 53 *)
  
let maListe = 2::5::4::3::6::[];;
let maPile = [2; 5; 4; 3; 6];;
  
let rec listeToPile (l : int list) : pile=
  match l with
  | [] -> []
  | t::reste -> empile t (listeToPile reste);;

listeToPile maListe;;

let rec pileToListe (pile : pile) : int list =
  match pile with
  | [] -> []
  | x -> let (element,restePile) = depile x in
	 element::(pileToListe restePile);;

pileToListe maPile;;

let teste_pile (l : int list) : bool =
  l=(pileToListe (listeToPile (l)));;   
  
teste_pile maListe;;

(*Exercice 54 *)

let rac ( l : 'a list ) : 'a * 'a list =
  match l with
  | [] -> failwith "Liste vide"
  | x -> let renversee = (renverse l []) in
	 match renversee with
	 | (t::reste) ->(t, (renverse reste []));;

(* A ameliorer avec rac recursif -> un seul appel à renverse *)  

rac maListe;;
    
(*Exercice 55*)
  
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


let (maFile : file) = file_vide;;
let (maFile : file) = enfile 5 maFile;;
let (maFile : file) = enfile 4 maFile;;
let (maFile : file) = enfile 3 maFile;;
let (element,maFile) = defile maFile;;
let (element,maFile) = defile maFile;;
let (element,maFile) = defile maFile;;  
let (element,maFile) = defile maFile;;

(* Exercice 56 *)

let maListe = 2::5::4::3::6::[];;
let maFile = [2; 5; 4; 3; 6];;
  
let rec listeToFile (l : int list) : file=
  match l with
  | [] -> []
  | t::reste -> enfile t (listeToFile reste);;

listeToFile maListe;;

let rec fileToListe (file : file) : int list =
  match file with
  | [] -> []
  | x -> let (element,resteFile) = defile x in
	 (fileToListe resteFile)@[element];;

fileToListe maFile;;

let teste_file (l : int list) : bool =
  l=(fileToListe (listeToFile (l)));;   
  
teste_file maListe;;

(* Exercice 57 *)

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

let rec getIemeElement ( i : int ) ( l : 'a list ) : 'a =
  let t::reste = l in
		match i with
  	| 0 -> t
		| x -> getIemeElement (x-1) reste;;
  

let rang ( rang : int )(l : 'a list) : 'a =
  let listeTriee = (tri_rapide l) in
	getIemeElement rang listeTriee;; 
  
	
rang 5 maListeAleatoire;;	
rang 20 maListeAleatoire;;	














