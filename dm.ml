(* Caractéristique *)

type nom = Tinkywinky | Laalaa | Po | Dipsy;;

type fortune = int;;

type etat = Bonheur | Faim | Sommeil ;;

type lieu = Maison_Tinkywinky | Maison_Laalaa | Maison_Po | Maison_Dipsy | Usine | Hotel | Restaurant ;;

type personnage = { nom : nom ;
		    mutable fortune : fortune;
		    mutable etat : etat;
		    mutable lieu : lieu;
		    maison : lieu; };;
  
(* Action *)

  
let dormir ( p : personnage) : unit =
  match p.etat with
  | Sommeil -> begin match (p.lieu,p.maison) with
	       | (x,y) when x=y -> p.etat <- Faim
	       | (Hotel,_) -> if p.fortune >= 50 then
				begin p.fortune <- p.fortune-50 ; p.etat <- Faim  end	 
			      else
				failwith "Le personnage n'a pas assez d'argent"
	       | (_,_) -> failwith "Le personnage ne peut pas dormir à cet endroit"
	     end
  | _ -> failwith "Le personnage n'as pas sommeil";;


let manger ( p : personnage) : unit =
  match p.etat with
  | Faim -> begin match p.lieu with
	       | Restaurant -> if p.fortune >= 20 then
				 begin
				   p.fortune <- p.fortune-20 ; p.etat <- Bonheur
				 end
			       else
				failwith "Le personnage n'a pas assez d'argent"
	       | _ -> failwith "Le personnage ne peut pas manger à cet endroit"
	     end
  | _ -> failwith "Le personnage n'as pas faim";;

 
let travailler ( p : personnage) ( duree : int ) : unit =
   match p.etat with
   | Sommeil -> failwith "Le personnage ne peut pas travailler car il a sommeil"
   | _ -> begin
	  match p.lieu with
	  | Usine -> if (duree >= 1 && duree <=10) then
		       begin
			 p.fortune <- p.fortune+duree*10 ;
			 if duree >= 5 then
			   p.etat <- Sommeil
		       end
		     else
		       failwith "Durée de travail incorrect"
		| _ -> failwith "Vous ne pouvez pas travailler à cet endroit"
	  end;;
  
	    


let deplacer ( p: personnage) ( lieu : lieu ) : unit =
  match (p.lieu,lieu) with
  | (x,y) when x=y -> failwith "Le personnage est déjà à ce lieu"
  | (_,_) -> p.lieu <- lieu;;

				 
(* Personnage *)
  
let ( tw : personnage ) = { nom = Tinkywinky;
			   fortune = 10;
			   etat =  Sommeil;
			   lieu = Maison_Tinkywinky ;
			   maison = Maison_Tinkywinky};;

  
let ( dp : personnage ) = { nom = Dipsy;
			   fortune = 100;
			   etat =  Bonheur;
			   lieu = Usine ;
			   maison = Maison_Dipsy };;
  
let ( la : personnage ) = { nom = Laalaa ;
			   fortune = 0;
			   etat = Sommeil;
			   lieu = Restaurant ;
			   maison = Maison_Laalaa };;


(* Situation *)
  
let rec invite_resto ( l : personnage list ) : personnage list =
  match l with
  | [] -> []
  | t::reste -> if t.etat = Faim then
		  t::(invite_resto reste)
		else
		  invite_resto reste;;
    
    
let rec rencontre ( l : personnage list ) : bool =
  let rec est_dans_ce_lieu ( l : personnage list ) ( lieu : lieu ) : bool =
    match l with
    | [] -> false
    | t::reste -> (t.lieu=lieu) || (est_dans_ce_lieu reste lieu)
  in
  match l with
  | [] -> false
  | t::reste -> (est_dans_ce_lieu reste t.lieu) || (rencontre reste);;
  
(* Fonction loin d'être optimiser*)



  
  
let rec rencontre2 ( l : personnage list ) : lieu option =
  let rec lieu_ou_il_y_a_2_personnes ( l : personnage list ) ( lieu : lieu ) : lieu option =
    match l with
    | [] -> None
    | t::reste -> if t.lieu=lieu then
		    Some lieu
		  else
		    (lieu_ou_il_y_a_2_personnes reste lieu)
  in
  match l with
  | [] -> None
  | t::reste -> let r = (lieu_ou_il_y_a_2_personnes reste t.lieu) in            
		if r != None then
		   r
		else
		  (rencontre2 reste);;


 
  
