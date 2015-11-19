(* Anthony Geourjon 
   Polytech RICM3 Gr.2

   Compte-rendu TP5 OCaml (Quadtree) 

 *)

(*Préliminaires *)
#load "qtparser.cmo"
open Qtparser

let smyley_base = load_qt "smyley_base.pgm" ;;
let taille = 64;;
let nombre_couleur = 1;;

(* Exercice 66 *)
let rec rot_pos ( a : quadtree ) : quadtree =
  match a with
  | Feuille(x) -> Feuille(x)
  | Noeud(br1,br2,br3,br4) -> Noeud((rot_pos br4),(rot_pos br1),(rot_pos br2),(rot_pos br3));;

let smyley_plus90  = rot_pos smyley_base;;
save_qt taille nombre_couleur smyley_plus90 "smyley_plus90.pgm";;

(* Exercice 67 *)  
let rec rot_neg ( a : quadtree ) : quadtree =
  match a with
  | Feuille(x) -> Feuille(x)
  | Noeud(br1,br2,br3,br4) -> Noeud((rot_neg br2),(rot_neg br3),(rot_neg br4),(rot_neg br1));;

let smyley_moins90  = rot_neg smyley_base;;
save_qt taille nombre_couleur smyley_moins90 "smyley_moins90.pgm";;  

(* Exercice 68 *)   
let rec miroir_hori ( a : quadtree ) : quadtree =
  match a with
  | Feuille(x) -> Feuille(x)
  | Noeud(br1,br2,br3,br4) -> Noeud((miroir_hori br4),(miroir_hori br3),(miroir_hori br2),(miroir_hori br1));;

let smyley_hori  = miroir_hori smyley_base;;
save_qt taille nombre_couleur smyley_hori "smyley_hori.pgm";;  

(* Exercice 69 *)  
let rec miroir_vert ( a : quadtree ) : quadtree =
  match a with
  | Feuille(x) -> Feuille(x)
  | Noeud(br1,br2,br3,br4) -> Noeud((miroir_vert br2),(miroir_vert br1),(miroir_vert br3),(miroir_vert br4));;

let smyley_vert  = miroir_vert smyley_base;;
save_qt taille nombre_couleur smyley_vert "smyley_vert.pgm";;

   
(* Exercice 70 *)
let rec inversion_video (a: quadtree ) ( m : int ) : quadtree=
  match a with
  | Feuille(x) -> Feuille(m-x)
  | Noeud(br1,br2,br3,br4) -> Noeud( (inversion_video br1 m),(inversion_video br2 m),(inversion_video br3 m), (inversion_video br4 m));;

let smyley_ton_inverse = inversion_video smyley_base nombre_couleur ;;
save_qt taille nombre_couleur smyley_ton_inverse "smyley_ton_inverse.pgm";; 

(* Exercice 71 *)
let rec max_gris ( a : quadtree ) : int =
  let max4entrees (v1 : int)(v2 : int)(v3 : int)(v4 : int) : int =
    max v1 (max v2 (max v3 v4)) in
  match a with
  | Feuille(x) -> x
  | Noeud(br1,br2,br3,br4) -> max4entrees (max_gris br1) (max_gris br2) (max_gris br3) (max_gris br4);; 

max_gris smyley_base;;

(* Exercice 72 *)
let rec min_quad ( a : quadtree ) : quadtree =
  match a with
  | Feuille(x) -> Feuille(x)
  | Noeud(Feuille(f1),Feuille(f2),Feuille(f3),Feuille(f4)) -> if (f1=f2) && (f2=f3) && (f3=f4) then
								Feuille(f1)
							      else
								Noeud(Feuille(f1),Feuille(f2),Feuille(f3),Feuille(f4))
  | Noeud(br1,br2,br3,br4) -> let m1 = (min_quad br1) and m2 = (min_quad br2) and m3 = (min_quad br3) and m4 = (min_quad br4) in
			      match (m1,m2,m3,m4) with
			      | (Feuille(f1),Feuille(f2),Feuille(f3),Feuille(f4)) -> if (f1=f2) && (f2=f3) && (f3=f4) then
										       Feuille(f1)
										     else
										       Noeud(Feuille(f1),Feuille(f2),Feuille(f3),Feuille(f4))
			      | _ -> Noeud(m1,m2,m3,m4);; 							

let image_uniforme = Noeud(Feuille(1),Feuille(1),Feuille(1),Feuille(1));;

min_quad image_uniforme;;

let smyley_base_minimise = min_quad smyley_base;;
save_qt taille nombre_couleur smyley_base_minimise "smyley_base_minimise.pgm";;   

(* Exercice 73 *)  

Array.length;;
Array.get;;    
Array.make;;
Array.set;;    

let get (t : int array array) ( x : int ) ( y : int) : int =
  let colonne = (Array.get t x) in
  Array.get colonne y ;;
  
	       
let tab_vers_quad ( t : int array array) : quadtree =
  let rec sub_tab_vers_quad ( t : int array array) ( bih : int) ( bsh : int) ( biv : int) ( bsv : int ) : quadtree =
    let df = ((bsh-bih)+(bsv-biv)) in
    match df with
    | 0 -> Feuille(get t 0 0)
    | 2 -> Noeud(Feuille(get t bih biv),
		 Feuille(get t bsh biv),
		 Feuille(get t bsh bsv),
		 Feuille(get t bih bsv))
    | _ -> Noeud(
	       (sub_tab_vers_quad t        bih            ((bsh-bih)/2+bih)         biv          ((bsv-biv)/2+biv)  ),
	       (sub_tab_vers_quad t ((bsh-bih)/2+1+bih)          bsh                biv          ((bsv-biv)/2+biv)  ),
	       (sub_tab_vers_quad t ((bsh-bih)/2+1+bih)          bsh         ((bsv-biv)/2+1+biv)         bsv      ),
	       (sub_tab_vers_quad t        bih            ((bsh-bih)/2+bih)  ((bsv-biv)/2+1+biv)         bsv      )
             ) in
  let taille = (Array.length t) in
  min_quad (sub_tab_vers_quad t 0 (taille-1) 0 (taille-1));;

let t = Array.make 8 (Array.make 8 1);;
let t2 = Array.make 2 (Array.make 2 1);;  
let t3 = Array.make 1 (Array.make 1 1);;
let t4 = Array.make 4 (Array.make 4 1);; 

tab_vers_quad t;;  
tab_vers_quad t2;;  
tab_vers_quad t3;;  
tab_vers_quad t4;;

(*Exercice 74*)
(* Question non réalisée *)

let set (t : int array array) ( x : int ) ( y : int) ( v : int ): unit =
  let colonne = (Array.get t x) in
  Array.set colonne y x ;;
  
let quad_vers_tab ( taille : int )( q : quadtree ) : int array array =
  let t = (Array.make taille (Array.make taille 0)) in
  let vide = (sub_quad_vers_tab q t 0 (taille-1) 0 (taille-1)) in
  t;;
	     
let rec sub_quad_tab ( q : quadtree) ( t : int array array ) ( bih : int) ( bsh : int) ( biv : int) ( bsv : int ) : unit =
  let dif = ((bsh-bih)+(bsv-biv)) in
  match (q,dif) with
  | (feuille(x),0) -> set t bih biv x
  | (feuillle(x),_) -> for i = bih to bsh do
			 for j = biv to bsv do
			   set t i j x
			 done
	               done
  | ((Noeud(br1,br2,br3,br4)),_) ->  
