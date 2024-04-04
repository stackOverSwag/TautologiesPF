type fp = X of int | Vrai | Faux | Et of fp * fp | Ou of fp * fp | Imp of fp * fp | Equiv of fp * fp | Non of fp ;;

let rec elim_equiv p = match p with
		       X(i)->X(i)
		       |Vrai->Vrai
		       |Faux->Faux
		       |Et(pa,pb)->Et(elim_equiv pa,elim_equiv pb)
		       |Ou(pa,pb)->Ou(elim_equiv pa,elim_equiv pb)
		       |Imp(pa,pb)->Imp(elim_equiv pa,elim_equiv pb)
		       |Equiv(pa,pb)->Et(Imp((elim_equiv pa), (elim_equiv pb)),Imp((elim_equiv pb), (elim_equiv pa)))
		       |Non(pa)->Non(elim_equiv pa);;
		   
let rec elim_imp p = match p with
		 X(i)->X(i)
		 |Vrai->Vrai
		 |Faux->Faux
		 |Et(pa,pb)->Et(elim_imp pa,elim_imp pb)
		 |Ou(pa,pb)->Ou(elim_imp pa,elim_imp pb)
		 |Imp(pa,pb)->Ou(Non((elim_imp pa)), (elim_imp pb))
		 |Equiv(pa,pb)->Equiv(elim_imp pa,elim_imp pb)
		 |Non(pa)->Non(pa);;
		 
let rec repousse_neg p = match p with
		 	 X(i)->X(i)
		 	 |Vrai->Vrai
		 	 |Faux->Faux
		 	 |Et(pa,pb)->Et(repousse_neg pa, repousse_neg pb)
		 	 |Ou(pa,pb)->Ou(repousse_neg pa, repousse_neg pb)
		 	 |Imp(pa,pb)->Imp(repousse_neg pa, repousse_neg pb)
		 	 |Equiv(pa,pb)->Equiv(repousse_neg pa, repousse_neg pb)
		 	 |Non(pa)->match pa with
		 	 	   X(i)->X(i)
		 		   |Vrai->Faux
		 		   |Faux->Vrai
		 		   |Et(pa,pb)->Ou(repousse_neg (Non(pa)), repousse_neg (Non(pb)))
		 		   |Ou(pa,pb)->Et(repousse_neg (Non(pa)), repousse_neg (Non(pb)))
		 		   |Imp(pa,pb)->Ou(repousse_neg (Non(pa)), repousse_neg pb)
		 		   |Equiv(pa,pb)->Ou(repousse_neg(Non(Imp(pa,pb))),repousse_neg(Imp(pb,pa)))
		 		   |Non(pa)->repousse_neg(Non(pa));;

let rec inverse_ou_et p = match p with (*Finir cette partie.*)
			  X(i)->X(i)
		 	 |Vrai->Vrai
		 	 |Faux->Faux
		 	 |Et(pa,pb)->Et(pa,pb)
		 	 |Ou(pa,pb)->Ou(pa,pb)
		 	 |Imp(pa,pb)->Imp(pa,pb)
		 	 |Equiv(pa,pb)->Equiv(pa,pb)
		 	 |Non(pa)->Non(pa)
