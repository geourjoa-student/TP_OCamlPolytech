let pi = 4. *. atan 1.

(** Point ou vecteur. *)
type point = { x : float; y : float }


(** Surface. *)
type surface =
  | Circle of point * float (** Cercle : centre et rayon *)
  | Line of point * point   (** Segment : deux extremites *)
  | Polygon of point list   (** Polygone plein : listes des sommets *)


  (** Constructeur de point. *)
let point ( a : float) (b : float) : point =
  { x=a; y=b}


(** Opérateurs infixes sur les vecteurs. *)

let ( +| ) (p1 : point) (p2 : point) : point =
  {x=(p1.x+.p2.x); y=(p1.y+.p2.y)}

let ( -| ) (p1 : point) (p2 : point) : point =
  {x=(p1.x-.p2.x); y=(p1.y-.p2.y)}

let ( ~| ) (p : point) : point =
  {x=(p.y); y=(p.x)}


let ( *| ) (a : float) (p : point) : point =
  {x=(a*.p.y); y=(a*.p.x)}


let milieu (p1 : point) (p2 : point) : point =
  { x=((p1.x+.p2.x)/.2.); y=((p1.y+.p2.y)/.2.)}

let normal (p : point) : point =
  { x=(-.p.y); y=(p.x)}

let dot ( p1 : point) (p2 : point) : float =
  (p1.x*.p2.x) +. (p1.y*.p2.y)

let sqdot (p : point) : float =
  (p.x *. p.x)+.(p.y *. p.y)

		  
let length (p : point) : float =
  sqrt ((p.x *. p.x)+.(p.y *. p.y))

let distance ( p1 : point) (p2 : point) : float =
  sqrt ( (p1.x+.p2.x)*. (p1.x+.p2.x) +. (p1.y+.p2.y) *. (p1.y+.p2.y))
       
let unitise ( p : point) : point =
  { x= (p.x /. (length p)) ; y= (p.y /. (length p))    }
	   
  
let rotate ( p : point) ( a : float ) : point =
  { x= ((p.x *. cos(a)) -. (p.y *. sin(a))) ; y= ((p.x *. sin(a)) -. (p.y *. cos(a)))}

   
