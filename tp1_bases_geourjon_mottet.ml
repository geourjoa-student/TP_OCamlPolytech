(* Mottet Loic & Geourjon Anthony

APF - Compte rendu TP1

***********************************)

(* Exercice 1: *)

(*
# val r : int = 42
# val a : int = 0
Erreur, x n'est pas défini. 
# val u : int = 2
# val l : int = 35
*)

(* Exercice 2: *)

let f1 =  fun x -> fun y -> fun z ->  x*y*z;;
let f2=  fun x -> fun y -> fun z ->  x+y+z;;

(* Exercice 3: *)

let somme = fun x -> fun y -> x+y;;
let sup = fun (x:int) -> fun y -> x>y;;
(*-f3 est une fonction à deux parametres dont le premier est une fonction qui prend un int en parametre et renvoie un boolée,. le second est un entier et la fonction retourne un bool. Le type de f3 est :
val f3 : (int -> bool) -> int -> bool = <fun>
-f4 est une fonction à deux paramretres dont le premier est une fonction qui prend un int en parametre et renvoie un int, le second est un entier et la fonction retourne un int. Le type de f4 est :
val f4 : (int -> int) -> int -> int = <fun>
-f5 est une fonction qui prend un parametre et qui retourne deux valeurs de ce même parametre. C'est évidement impossible et cela provoque une erreur.
*)

(* Excercice 4: *) 

let f = fun i -> fun x -> x + i;;
let g = f 2;;

let f1 = fun a -> fun b -> b*a;;
let g1 = f1 a;; 

(* Exercice 5 : *)

let positif = fun x -> x>=0;;
(*Appel : positif (-2);;*)
let paire= fun x -> (x mod 2)=0;;
(* Appel : paire 2;; *)
let triplet a b c = (a**2)=(b**2+c**2);; où b^2 + c^2 = a^2
(*Appel 5 3 4 -> vrai*)
let memeSigne a b = (positif a)=(positif b);;
(*Appel : memeSigne (-2) 3*)

(* Exercice 6 :*)

let min2entiers (a : int ) (b : int ): int =
	if (a<b) then a 
	else b;;

min2entiers 4 2 ;;

(* Exercice 7 : *)

let max2entiers ( a : int ) (b : int ):int =
	if (a>b) then a else b;;
max2entiers 3 66;;

(* Exercice 8 : *)

let min3entiers ( a : int) (b : int) (c : int): int =
(min2entiers ( min2entiers a b) c) ;;

min3entiers 3 5 1

(* Exercice 9 : *)

type semaine = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche;;
let estwe (j : semaine ): bool = 
  j = Samedi || j= Dimanche;;

estwe Samedi;;

(* Exercice 10 : *)

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

(* Exercice 11: *)

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

(* Exercice 12: *)

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

(* Exercice 13 : *)

type point = { x : float; y : float };;

let distance (a: point) (b: point): float =
    sqrt (b.x-.a.x)**2.+.(b.y-.a.y)**2.;;

let (a : point) = { x=1. ; y= 2.};;
let (b : point) = { x=3. ; y= 2.};;
distance a b;;


type segment = { x : float; y : float };;

let milieu (a: segment) : float =
  (a.y-.a.x)/.2.+.a.x;;

let (a : segment) = { x=2. ; y= 5.};;
milieu a ;;

type point = { x : float; y : float };;

type vecteur = { x : float; y : float };;

let (a : point) = { x=2. ; y= 3.};;
let (b : point) = { x=2. ; y= 5.};;

let creeVecteur (a:point) (b:point) : vecteur =
  { x=b.x-.a.x ; y=b.y-.a.y };;

type point = { x : float; y : float };;
type vecteur = { x : float; y : float };;
let creeVecteur (a:point) (b:point) : vecteur =
  { x=b.x-.a.x ; y=b.y-.a.y };;

type droite = { p : point ; v :vecteur};;

let (d : droite) = { p={ x=1. ; y= 1.} ; v=creeVecteur { x=2. ; y= 5.}  { x=2. ; y= 3.}}

type equaDroite = { a : float ; b : float };;
(* let equationDeDroite (d: droite) : equaDroite = { a=d.v.y/.d.v.x ; b= ?} *)

(* Lorsqu'on a determiné l'équation ax+b de chacune des droites, on obtient un système. En résolvant x et y, on obtient le point d'intersection. Nous n'avons pas su le transcrire en OCaml.*)

