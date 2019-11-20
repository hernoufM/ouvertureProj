
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

type 'a abr_compresse =
    | Empty
    | Node of 'a * (('a abr_compresse) ref * int) * (('a abr_compresse) ref * int);;

let rec insert tree a =
  match tree with
    | Empty  -> Node (a,((ref Empty),0),((ref Empty),0))
    | Node(v,(refL,etL),(refR,etR))  ->
        if (a<v) 
        then Node (v,(ref (insert !refL a),etL),(refR,etR))
        else Node (v,(refL,etL),(ref (insert !refR a) ,etR));;


let tree1 = Empty;;
let tree2 = insert tree1 2;;
let tree3 = insert tree2 1;;
let tree4 = insert tree3 3;;

let liste_to_arbre_comp l =
  let rec aux l tree=
    match l with
      | [] -> tree
      | x::q -> aux q (insert tree x)
  in aux l Empty;;

let arbre_random = liste_to_arbre_comp (gen_permutation 5);;


(*'a c'est  le type qui est ((int*list int) list)*)
