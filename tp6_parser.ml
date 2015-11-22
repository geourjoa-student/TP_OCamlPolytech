(* --------------------------------------- *)
(* Geourjon Anthony / Savary Rémy - GR.2   *)
(* TP6 Analyse syntaxique                  *)

#load "dynlink.cma";;
#load "camlp4o.cma";;

(* Exercice 79 *)

let digit a = int_of_char a - int_of_char '0';;

let rec horner (x : int)( s : char Stream.t ) : int =
  match s with parser
  | [<''0'..'9' as a >] -> horner (10*x + digit a) s
  | [<>] -> x;;

let s = "123 181";;
horner 0 (Stream.of_string s);;
horner 42 (Stream.of_string s);; 

(* Exercice 80 *)

type couleur = Nil | Blanc | Noir;; 
type coup = Coup of couleur*int*int;;
type plateau = Nil | Plateau of coup*plateau;;

(* Exercice 81 *)

type token = ParO | ParF | Int of int | Couleur of couleur;;

let rec annalex_partie (s:char Stream.t): token Stream.t =
  match s with parser
  | [<''(';s = annalex_partie>] -> [<'ParO;s>]
  | [<'')';s = annalex_partie>] -> [<'ParF;s>]
  | [<''b';''l';''a';''n';''c';s=annalex_partie>] -> [<'Couleur(Blanc);s>]
  | [<''n';''o';''i';''r';s=annalex_partie>] -> [<'Couleur(Noir);s>]
  | [<''0'..'9' as c;i=horner(digit c);s=annalex_partie>] -> [<'Int(i);s>]
  | [<'' '| '\n'| '\t' ;s=annalex_partie>] -> s
  | [<>] -> [<>];;


let fichierPartieConnect6 = "(19 19)(noir 3 5) (blanc 5      8 )    (blanc 5  9)  (        noir 3 4)(noir 3 6)  ";;
let maPartie = annalex_partie (Stream.of_string fichierPartieConnect6);; 

(* Exercice 82 *)

let rec sub_lit_partie (s:token Stream.t) : plateau =
  match s with parser
  | [< 'ParO; 'Couleur(couleur); 'Int(x); 'Int(y); 'ParF; s = sub_lit_partie >] -> Plateau(Coup(couleur,x,y),s)
  | [<>] -> Nil
  | [<'_>]-> failwith "Flux non valide";;

let rec lit_partie  (s:token Stream.t) : (int*int)*plateau =
  match s with parser
  | [< 'ParO; 'Int(x); 'Int(y); 'ParF; s = sub_lit_partie >] -> ((x,y),s)
  | [<>]->((0,0),Nil)
  | [<'_>]-> failwith "Flux non valide";;


let (taillePlateau, lesCoups) = lit_partie (annalex_partie (Stream.of_string fichierPartieConnect6));;

(* Exercice 83 *)

let rec joue_coup (plateau:plateau)(coup:coup) =
  match plateau with
  | Nil -> Plateau(coup,Nil)
  | Plateau(coup1,plateau1) -> Plateau(coup1,(joue_coup plateau1 coup));;

let monCoup = Coup(Noir,12,12);;

joue_coup lesCoups monCoup;;

