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
   
