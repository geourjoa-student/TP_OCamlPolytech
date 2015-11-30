let ( a : Geo.point ) = ( Geo.point 0.3 0.5) ;;
let ( b : Geo.point ) = ( Geo.point 0.7 0.5 );;
 
let (i : int) = 5;;		      




    s1     s2
     *******
     *     *
p1   *     *    p2
******t1 t2******
     *     *
     *     *
     *******
    s3     s4



  
let nouveaux_points : ( p1 : Geo.point)  ( p2 : Geo.point) :  Geo.point* Geo.point* Geo.point* Geo.point* Geo.point*point =
  let (t1 : Geo.point) =  (Geo.point ( p1.Geo.x+.((p2.Geo.x-.p1.Geo.x)/.3.))        (p1.Geo.y+.((p2.Geo.y-.p1.Geo.y)/.3.)))   in
  let (t2:  Geo.point) =  (Geo.point ( p1.Geo.x+.(2.*.(p2.Geo.x-.p1.Geo.x)/.3.))  (p1.Geo.y+.(2.*.(p2.Geo.y-.p1.Geo.y)/.3.)))     in
  let (s1:  Geo.point) = (* nous n'avons pas réussi à trtouver comment calculer les coordonées de point *)  in
  let (s2 : Geo.point) = (* idem *) in
  let (s3 : Geo.point) = (* idem *) in
  let (s4 : Geo.point) = (* idem *) in
  
  (p1,p2,t1,t2,s1,s2,s3,s4);;  
    
  


let rec peano (p1 :  Geo.point) (p2 :  Geo.point) ( i : int ) : Geo.surface list =
  match i with
  | 0 -> []
  | 1 -> let (a,b,c,d,e,f,g,h) = (nouveaux_points p1 p2) in
	 [(Geo.Line ( a, c));  (Geo.Line ( c, e)); (Geo.Line ( e, f)); (Geo.Line (f, d) ; (Geo.Line ( d, b)); (Geo.Line ( c, g)); (Geo.Line (g, h) ; (Geo.Line ( h, d)) ]	 
  | x -> let (a,b,c,d,e,f,g,h) = (nouveaux_points p1 p2) in
	 (peano a c (x-1)) @ (peano c e (x-1)) @ (peano e f (x-1)) @ (peano f d (x-1)) @ (peano d b (x-1)) @ (peano c g (x-1)) @ (peano g h (x-1)) @ (peano h d (x-1));;

Aff.draw "PEANO " (peano a b i);;  
