type cond = | W of int | Vrai_bis | Faux_bis | Si of cond * cond * cond 
type prop = | V of int | Vrai | Faux | Et of prop * prop | Ou of prop * prop | Non of prop | Imp of prop * prop | Equiv of prop * prop 
type env = (int * bool) list;;
(* P1 *)
let rec prop_to_cond prop =
  match prop with
  | V(i) -> W(i)
  | Vrai -> Vrai_bis | Faux -> Faux_bis
  | Et(p1, p2) -> Si(prop_to_cond p1, prop_to_cond p2, Vrai_bis)
  | Ou(p1, p2) -> Si(prop_to_cond p1, Vrai_bis, prop_to_cond p2)
  | Non(p) -> Si(prop_to_cond p, Faux_bis, Vrai_bis)
  | Imp(p1, p2) -> prop_to_cond(Ou(Non(p1),p2))
  | Equiv(p1, p2) -> prop_to_cond(Et(Imp(p1,p2),Imp(p2,p1))) 

(* P2 *)
let rec for_nor p = 
  match p with
  | W(i) -> W(i)
  | Vrai_bis | Faux_bis -> p
  | Si(p1, p2, p3) -> match (p1,p2,p3) with
    |(W(i),p2,p3)->Si(W(i),for_nor p2,for_nor p3)
    |(Vrai_bis,p2,p3)->Si(Vrai_bis,for_nor p2,for_nor p3)
    |(Faux_bis,p2,p3)->Si(Faux_bis,for_nor p2,for_nor p3)
    |(Si(pa,pb,pc),p2,p3)-> for_nor (Si(pa,for_nor (Si(pb,p2,p3)),for_nor (Si(pc,p2,p3))));; 

(* P3 *)
let rec cherche_couple e i = match e with
    []->false,false
  |(x,b)::e0->if x=i
      then true,b
      else cherche_couple e0 i;;


let rec eval f e = match f with
    W(i)-> let a,b = cherche_couple e i in
      if a = true
      then b
      else false
  |Vrai_bis->true
  |Faux_bis->false
  |Si(g,h,k)-> match g with
      W(i)-> let a,b = cherche_couple e i in
        if a = true
        then if b = true
          then eval h e
          else eval k e
        else let e1 = (i,true) :: e in
          let e2 = (i,false) :: e in
          eval h e1 && eval k e2
    |Vrai_bis->eval h e
    |Faux_bis->eval k e
    |Si(m,n,o)-> false;;



(* Convertir une proposition en chaîne de caractères *)
let string_of_prop prop =  
  let rec string_of_prop_aux prop =
    match prop with
    | V(i) -> Printf.sprintf "V(%d)" i
    | Vrai -> "Vrai"
    | Faux -> "Faux"
    | Et(p1, p2) -> Printf.sprintf "(%s) ∧ (%s)" (string_of_prop_aux p1) (string_of_prop_aux p2)
    | Ou(p1, p2) -> Printf.sprintf "(%s) ∨ (%s)" (string_of_prop_aux p1) (string_of_prop_aux p2)
    | Non(p) -> Printf.sprintf "¬(%s)" (string_of_prop_aux p)
    | Imp(p1, p2) -> Printf.sprintf "(%s) → (%s)" (string_of_prop_aux p1) (string_of_prop_aux p2)
    | Equiv(p1, p2) -> Printf.sprintf "(%s) ⇔ (%s)" (string_of_prop_aux p1) (string_of_prop_aux p2)
  in
  string_of_prop_aux prop

(* Convertir une condition en chaîne de caractères *)
let string_of_cond cond =
  let rec string_of_cond_aux cond =
    match cond with
    | W(i) -> Printf.sprintf "W(%d)" i
    | Vrai_bis -> "Vrai_bis"
    | Faux_bis -> "Faux_bis"
    | Si(p1, p2, p3) -> Printf.sprintf "Si(%s, %s, %s)" (string_of_cond_aux p1) (string_of_cond_aux p2) (string_of_cond_aux p3)
  in
  string_of_cond_aux cond
  
let string_of_result result =
  match result with 
  | true -> "True"
  | false -> "False"

(* Fonction de test *)
let test_prop_to_cond_and_for_nor () =
  (* Exemples de propositions *)
  let prop1 = Vrai in
  let prop2 = Et( V(1),Vrai) in

  (* Convertir les propositions en conditions *)
  let cond1 = prop_to_cond prop1 in
  let cond2 = prop_to_cond prop2 in

  (* Normaliser les conditions *)
  let nor_cond1 = for_nor cond1 in
  let nor_cond2 = for_nor cond2 in
  
  let eval_cond1 = eval nor_cond1 [] in
  let eval_cond2 = eval nor_cond2 [] in

  (* Afficher les résultats *)
  print_endline "Propositions originales:";
  print_endline (Printf.sprintf "Proposition 1: %s" (string_of_prop prop1));
  print_endline (Printf.sprintf "Proposition 2: %s" (string_of_prop prop2));
  print_newline ();

  print_endline "Conditions converties:";
  print_endline (Printf.sprintf "Condition 1: %s" (string_of_cond cond1));
  print_endline (Printf.sprintf "Condition 2: %s" (string_of_cond cond2));
  print_newline ();

  print_endline "Conditions normalisées:";
  print_endline (Printf.sprintf "Condition normalisée 1: %s" (string_of_cond nor_cond1));
  print_endline (Printf.sprintf "Condition normalisée 2: %s" (string_of_cond nor_cond2)); 

(* Afficher le résultat *)
  if eval_cond1 then
    print_endline "La proposition 1 est une tautologie."
  else
    print_endline "La proposition 1 est pas une tautologie.";
  
      
(* Afficher le résultat *)
  if eval_cond2 then
    print_endline "La proposition 2 est une tautologie."
  else
    print_endline "La proposition 2 est pas une tautologie."
;;

(* Exécuter le test *)
test_prop_to_cond_and_for_nor ()
