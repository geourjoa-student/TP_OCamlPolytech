let ( a : Geo.point ) = ( Geo.point 0.3 0.5) ;;
let ( b : Geo.point ) = ( Geo.point 0.5 0.8461 );;
let ( c : Geo.point ) = ( Geo.point 0.7 0.5 );;
 
let (i : int) = 5;;		      


let nouveaux_points ( p1 : Geo.point)  ( p2 : Geo.point) :  Geo.point* Geo.point* Geo.point* Geo.point* Geo.point =
  let (t1 : Geo.point) =  (Geo.point ( p1.Geo.x+.((p2.Geo.x-.p1.Geo.x)/.3.))        (p1.Geo.y+.((p2.Geo.y-.p1.Geo.y)/.3.)))   in
  let (t2:  Geo.point) =  (Geo.point ( p1.Geo.x+.(2.*.(p2.Geo.x-.p1.Geo.x)/.3.))  (p1.Geo.y+.(2.*.(p2.Geo.y-.p1.Geo.y)/.3.)))     in
  let (s:  Geo.point) =   (Geo.point ( ((t1.Geo.x+.t2.Geo.x)*.cos(Geo.pi/.3.))   -. ((t2.Geo.y-.t1.Geo.y)*.sin(Geo.pi/.3.)))
				     ((t1.Geo.y+.t2.Geo.y)*.cos(Geo.pi/.3.)+.(t2.Geo.x-.t1.Geo.x)*.sin(Geo.pi/.3.))) in
  (p1,p2,t1,t2,s);;  
    
  
let rec koch (p1 :  Geo.point) (p2 :  Geo.point) ( i : int ) : Geo.surface list =
  match i with
  | 0 -> []
  | 1 -> let (a,b,c,d,e) = (nouveaux_points p1 p2) in
	 [(Geo.Line ( a, c));  (Geo.Line ( c, e)); (Geo.Line ( e, d)); (Geo.Line ( d, b))]	 
  | x -> let (a,b,c,d,e) = (nouveaux_points p1 p2) in
	 (koch a c (x-1)) @ (koch c e (x-1)) @ (koch e d (x-1)) @ (koch d b (x-1));;


(*let () =  Aff.draw "ESSAI" [Geo.Line( a , b); 
			    Geo.Line( b , c);
			    Geo.Line( c , a) ];;  *)

Aff.draw "KOCH " ( (koch a b i) @ (koch b c i) @ (koch c a i ));;





