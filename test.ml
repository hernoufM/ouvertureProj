

(************************ Question 1.1 ************************)

(* to generate a random number between 1 and the length of the list *)
let random_number_in_list l =
  1 + Random.int (List.length l) ;;

(* return l without the rth element  *)
let l_without_r l r =
  let rec aux l r res=
    match l with
    |[]-> res
    |h::t -> if (r==1) then (aux t (r-1) res)
      else (aux t (r-1) (res@[h]))
  in aux l r [];;

(* return p with r on the head   *)
let r_on_the_head p r = [r]@p;;

let extraction_alea l p =
  (* The function randomly chooses an integer between 1 and the size of l *)
  let r1 = random_number_in_list l
  in
  (* c1 will be the list l without the r1th element *)
  let c1 = l_without_r l r1
  in
  (* getting the value of the r1th element of the list l  *)
  let r2 = List.nth l (r1-1)
  in
  (* putting the value on the head of the list p *)
  let c2 = r_on_the_head p r2
  (* returning the couple *)
  in (c1,c2);;


(************************ Question 1.2 ************************)

(* This function generates a list of integers from 1 to n (L is sorted in  *)
(*   ascending order) *)
let generate_list_1_to_n n =
  let rec aux n res =
    match n with
    | 1 -> 1::res
    | n -> aux (n-1) (n::res)
  in aux n [];;

(* this function completely empties the list l and populates the list p by *)
(* calling f (in our case it will call extraction_alea) *)
let empty_list_with_alea l p f =
  let rec aux l p =
    match l with
    |[]-> p
    |_ -> let c = f l p
      in aux (fst c) ( snd c )
  in aux l p;;


(* this function returns a random list with values between 1 and n *)
let gen_permutation n =
  let l = generate_list_1_to_n n
  in
  let p = []
  in
  empty_list_with_alea l p extraction_alea;;

(* ctrl y  et ctrl f*)
(************************ Question 1.3 ************************)
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;
(*  left son        right son*)

let insert_a tree a =
  let rec aux tree =
    match tree with
    |Empty -> Node (a,Empty,Empty)
    |Node(r,left,right) ->
      if (a<r) then Node (r,aux left, right)
      else Node (r,left, aux right)
  in aux tree ;;

let liste_to_arbre l =
  let rec aux l tree=
    match l with
    | [] -> tree
    | x::q -> aux q (insert_a tree x)
  in aux l Empty;;

(************************ Question 2.4 ************************)

let convert_tree_to_string tree =
  let rec aux tree res =
    match tree with
    |Empty -> res
    |Node(r,left,right) ->
      "("^(aux left "")^")"^(aux right "")
  in aux tree "";;

let l=liste_to_arbre (gen_permutation 9);;
convert_tree_to_string (l);;

let listes_construction tree =
  let res1 = ref [] and res2 = ref [] in
  let rec aux tree res1 res2 =
    match tree with
    | Empty -> ([],[])
    | Node(v,ag,ad) ->  let rec appartient tree liste =
                          match liste with
                          | [] -> false
                          | (x,stringX)::q -> if stringX=(convert_tree_to_string tree)
                            then true
                            else appartient tree q
      in (if not (appartient tree !res1) then res1:= (List.append !res1 [(v, (convert_tree_to_string tree))])
          else
            let rec racine_eq tree liste =
              match liste with
              | [] -> -1
              | (x,stringX)::q -> if stringX=(convert_tree_to_string tree)
                then x
                else racine_eq tree q
            in res2:= (List.append !res2 [(v,racine_eq tree !res1)]));
      (if not (ag = Empty) then let (a,b) = aux ag res1 res2 in res1:=a; res2:=b);
      (if not (ad = Empty) then let (a,b) = aux ad res1 res2 in res1:=a; res2:=b);
      (!res1,!res2);
  in
  aux tree res1 res2;;

let v = [4;2;1;3;8;6;5;7;9];;
listes_construction (liste_to_arbre v);;

type valeurABRC_listes = (int* int list) list;;

type abrc_listes =
  | EmptyABRC
  | NodeABRC of valeurABRC_listes * (abrc_listes ref * int) * (abrc_listes ref * int);;

let rec insert tree a =
  match tree with
  | EmptyABRC  -> NodeABRC ([(a,[])],((ref EmptyABRC),0),((ref EmptyABRC),0))
  | NodeABRC(v,(refL,etL),(refR,etR))  ->
    if (a< fst(List.hd(v)))
    then NodeABRC (v,(ref (insert !refL a),etL),(refR,etR))
    else NodeABRC (v,(refL,etL),(ref (insert !refR a) ,etR));;

let liste_to_abrc l =
  let rec insert_liste tree l =
    match l with
    | [] -> tree
    | x::q -> insert_liste (insert tree x) q
  in insert_liste EmptyABRC l;;

let rec ref_node_abrc abrc v =
  match abrc with
  | EmptyABRC -> raise Not_found
  | NodeABRC(x,(refL,etL),(refR,etR)) ->
    if (v< fst(List.hd(x))) then (match !refL with
    | EmptyABRC -> raise Not_found
    | NodeABRC(fils,_,_) -> if (fst (List.hd fils)) = v then refL else ref_node_abrc !refL v)
    else (match !refR with
    | EmptyABRC -> raise Not_found
    | NodeABRC(fils,_,_) -> if (fst (List.hd fils)) = v then refR else ref_node_abrc !refR v);;

let liste_refs l abrc = List.map (ref_node_abrc abrc) l;;


let rec supperier_all v l =
  match l with
  | [] -> true
  | x::q -> if(fst x< fst v) then supperier_all v q else false;;


let rec inferier_all v l =
  match l with
  | [] -> true
  | x::q -> if(fst x> fst v) then true else false;;

let rec insert_ordered_list v l =
  match l with
  | [] -> [v]
  | x::q -> if(fst x< fst v) then x::(insert_ordered_list v q) else  v::x::q

let etiq = ref 0;;
let gen_etiq () = etiq:=!etiq+1; !etiq;;
let relancer_gen() = etiq:=0;;

let get_value_etiq abrc liste_etiq =
  match abrc with
  | EmptyABRC -> raise Not_found
  |  NodeABRC(x,(refL,etL),(refR,etR))->
    let rec aux l =
match l with
| [] -> -1
| x::q -> let rec liste_egaux l1 l2 = match l1,l2 with
    | ([],[]) -> true
    | ([],x::q) -> false
    | (x::q,[]) -> false
    | (x1::q1, x2::q2) -> if (x1=x2) then liste_egaux q1 q2 else false
  in if (liste_egaux (snd x) liste_etiq) then fst x else aux q
in aux x;;


let insert_abrc_etiq v reference abrc =
  if abrc = EmptyABRC then insert abrc v
  else
    let rec aux abrc listEtiq=
      print_int(v); print_endline("---");
      match abrc with
      | EmptyABRC -> print_endline("Empty");raise Not_found
      | NodeABRC(x,(refL,etL),(refR,etR)) ->
        print_endline("Node "^(string_of_int (fst (List.hd  x)))^"--");
        let valeur = get_value_etiq abrc listEtiq in
        if(valeur>=0) then
          (if(valeur>v) then
            ((print_endline("valeur>v");
            let etL = if(!refL=EmptyABRC) then gen_etiq() else etL and
                refL=if(!refL=EmptyABRC) then reference else refL in
            refL:=(aux !refL (if etL =0 then listEtiq else List.append listEtiq [etL]));
            NodeABRC(x, (refL, etL),(refR, etR))))
          else
            (print_endline("valeur<v");
             let etR = if(!refR=EmptyABRC) then gen_etiq() else etR and
               refR=if(!refR=EmptyABRC) then reference else refR in
           refR:=(aux !refR (if etR =0 then listEtiq else List.append listEtiq [etR]));
            NodeABRC(x, (refL, etL), (refR, etR))))
        else
          (print_endline("valeur<0");
           NodeABRC (insert_ordered_list (v,listEtiq) x, (refL,etL),(refR,etR)))
    in aux abrc [];;

let liste_to_abrc_etiq l abrc =
  let rec aux l abrc=
    match l with
    | [] -> abrc
    | x::q ->aux q (insert_abrc_etiq (fst x) (snd x) abrc)
  in aux l abrc

let compresse_abr_listes abr =
  let (listeConstr1, listeConstr2) = listes_construction abr
  in
  let abrc = liste_to_abrc (List.map fst listeConstr1)
  in
  let list_references = List.combine (List.map fst listeConstr2) (liste_refs (List.map snd listeConstr2) abrc) in
  let abrc2 =  liste_to_abrc_etiq list_references abrc in
  abrc2;;
