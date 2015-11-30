let ( a : Geo.point ) = (  Geo.point 0. 0.) ;;
let ( b : Geo.point ) = (  Geo.point 1. 0.);;
let (i : int) = 1;;		      


		  
let nouveaux_points ( p1 : Geo.point)  ( p2 : Geo.point) :  Geo.point* Geo.point* Geo.point* Geo.point* Geo.point =
  let (t1 : Geo.point) =  (Geo.point ( p1.Geo.x+.((p1.Geo.x+.p2.Geo.x)/.3.))        (p1.Geo.y+.(p1.Geo.y+.((p1.Geo.y+.p2.Geo.y)/.3.))))   in
  let (t2:  Geo.point) =  (Geo.point ( p1.Geo.x+.((p1.Geo.x+.p2.Geo.x)/.(2./.3.)))  (p1.Geo.y+.((p1.Geo.y+.p2.Geo.y)/.(2./.3.))))     in
  let (s:  Geo.point) =   (Geo.point ((t1.Geo.x+.t2.Geo.x)*.cos(60.)-.(t2.Geo.y+.t1.Geo.y)*.sin(60.)) ((t1.Geo.y+.t2.Geo.y)*.cos(60.)-.(t2.Geo.x+.t1.Geo.x)*.sin(60.))) in
  (p1,t1,s,t2,p2)

    
  
let rec koch (p1 :  Geo.point) (p2 :  Geo.point) ( i : int ) : Geo.surface list =
  match i with
  | 0 -> []
  | 1 -> let (a,b,c,d,e) = (nouveaux_points p1 p2) in
	 [(Geo.Line ( a, b));  (Geo.Line ( b, c)); (Geo.Line ( c, d)); (Geo.Line ( d, e))]	 
  | x -> let (a,b,c,d,e) = (nouveaux_points p1 p2) in
	 ((koch a b (x-1)) @ (koch b c (x-1)) @  (koch c d (x-1)) @  (koch d e (x-1)));;


Aff.draw "maFenetre" (koch a b i)
	   
