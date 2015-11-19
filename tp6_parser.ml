(* TP7 OCaml 

   Savary & Geourjon
   17/17/15

 *)

#load "dynlink.cma"
#load "camlp4o.cma"


let digit c = int_of_char c - int_of_char '0'      

let rec horner n = parser
  | [< ''0'..'9' as c ; n =  horner (10 * n + digit c)>] ->  n
  | [< >] -> n;;

let s = "123 1981";
  
horner 0 s;;  
