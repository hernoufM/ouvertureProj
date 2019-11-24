

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

(*Insertion d'une valeur dans ABR*)
let insert_a tree a =
  let rec aux tree =
    match tree with
    |Empty -> Node (a,Empty,Empty)
    |Node(r,left,right) ->
      if (a<r) then Node (r,aux left, right)
      else Node (r,left, aux right)
  in aux tree ;;

(*Insertion dans ABR de plusieurs valeures stockées dans liste *)
let liste_to_arbre l =
  let rec aux l tree=
    match l with
    | [] -> tree
    | x::q -> aux q (insert_a tree x)
  in aux l Empty;;

(************************ Question 2.4 ************************)
(*Convertion de ABR vers ça representation paranthesé*)
let convert_tree_to_string tree =
  let rec aux tree res =
    match tree with
    |Empty -> res
    |Node(r,left,right) ->
      "("^(aux left "")^")"^(aux right "")
  in aux tree "";;

(*Fonction qui prend ABR et retourne 2 listes (listeConstruction1, listeConstruction2):
  Premere liste contient les couples (valeur de racine d'ABR, expression paranthesé d'ABR).
  Deuxieme liste contient les couples (valeur de racine d'ABR, valuer de racine d'ABR ou on va inserer la premier valeur)
  Fonction effectue parcours prefixe d'ABR, et à chaque etape calcule l'expression paranthesé d'un arbre. Si on rencontre une nouvelle expression
  on stocke la couple du valeur de racine et d'expression paranthesé dans premiere liste. Sinon (si une telle expression deja existe dans
  premiere liste) on stocke l'assossiation du valeur de racine actuel à la valeur de racine d'un arbre qui a l'expression equivalent (qui est present dans
  premiere liste).
  A la fin dans les deux liste on obtient les valeurs qui n'auront pas les etiquetes (dans oremiere liste) et les valeur qui seront etiquettés
  (sans deuxieme liste)*)
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

(*Contenu d'en noeud ABR compressé. On y stocke les valeurs et les etiquettes assossié*)
type valeurABRC_listes = (int* int list) list;;

(*Structure d'un noeud ABR compressé. On y stocke la contenu d'un noeud, ainsi que la reference et l'etiquete assossié
  à cette reference du fils gauche. Pareil pour le fils droit. Etiquete sont des entriers. Etiquette 0 correspond à une reference
  non etiquetté*)
type abrc_listes =
  | EmptyABRC
  | NodeABRC of valeurABRC_listes * (abrc_listes ref * int) * (abrc_listes ref * int);;

(*Creation d'un noeud ABRC avec la valeur initial et references vers fils gauch et fils droit (qui sont EmptyABRC)
  ainsi que insertion de cette noeud dans bon endroit*)
let rec insert tree a =
  match tree with
  | EmptyABRC  -> NodeABRC ([(a,[])],((ref EmptyABRC),0),((ref EmptyABRC),0))
  | NodeABRC(v,(refL,etL),(refR,etR))  ->
    if (a< fst(List.hd(v)))
    then NodeABRC (v,(ref (insert !refL a),etL),(refR,etR))
    else NodeABRC (v,(refL,etL),(ref (insert !refR a) ,etR));;

(*Construction d'ABRC à partir d'un liste de valeurs initiales des noeuds*)
let liste_to_abrc l =
  let rec insert_liste tree l =
    match l with
    | [] -> tree
    | x::q -> insert_liste (insert tree x) q
  in insert_liste EmptyABRC l;;

(*Fonction qui prend une valeur initiale d'un noeud et ABRC comme arguments et retorune la reference vers cette noeud*)
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

(* Fonction qui prend la liste des valeurs initiales des noeuds et ABRC comme argument et retourne
   la liste des references vers ces noueuds correspondants*)
let liste_refs l abrc = List.map (ref_node_abrc abrc) l;;

(*Insertion d'une couple (valeur, etiquetes associé) dans la liste ordonné selon la valeur*)
let rec insert_ordered_list v l =
  match l with
  | [] -> [v]
  | x::q -> if(fst x< fst v) then x::(insert_ordered_list v q) else  v::x::q

let etiq = ref 0;;
(*Generateur des etiquetes*)
let gen_etiq () = etiq:=!etiq+1; !etiq;;
(*Fonction qui relance le generateur*)
let relancer_gen() = etiq:=0;;
(*Fonction qui verifie l'egualité de 2 listes*)
let rec liste_egaux l1 l2 = match l1,l2 with
  | ([],[]) -> true
  | ([],x::q) -> false
  | (x::q,[]) -> false
  | (x1::q1, x2::q2) -> if (x1=x2) then liste_egaux q1 q2 else false;;

(*Fonction qui prend le noeud d'ABRC et la liste des etiquetes comme argument. Fonction cherche la valeur associé aux etiquetes
  passées en parametre. Si elle trouve la valeur elle le retourne sinon elle retourne -1*)
let get_value_etiq abrc liste_etiq =
  match abrc with
  | EmptyABRC -> raise Not_found
  |  NodeABRC(x,(refL,etL),(refR,etR))->
    let rec aux l =
      match l with
      | [] -> -1
      | x::q -> if (liste_egaux (snd x) liste_etiq) then fst x else aux q
    in aux x;;

(*Insertion d'une valeur dans ABRC. Foncton prend la valeur, la reference vers noeud ou elle doit etre inséré
  et ABRC comme parametres. Elle parcours ABRC en sauvegardant tous les etiquetes par laquelle elle est passée (variable etiquettesVisitees).
  A chaque etape elle compare la valeur avec la valeur d'un noeud qui est associé au memes etiquettes que etiquettesVisitees. Si on arrive au
  EmptyABRC on creer nouvelle etiquete qui reference le noeud ou la valeur doit etre inséré. Si le noued ne contient pas une valeur
  qui est associé au memes etiquettes que etiquettesVisitees, on y insere la valeur associé au etiquettesVisitees*)
let insert_abrc_etiq v reference abrc =
  if abrc = EmptyABRC then insert abrc v
  else
    let rec aux abrc etiquettesVisitees=
      match abrc with
      | EmptyABRC -> raise Not_found
      | NodeABRC(x,(refL,etL),(refR,etR)) ->
        let valeur = get_value_etiq abrc etiquettesVisitees in
        if(valeur>=0) then
          (if(valeur>v) then
             ((let etL = if(!refL=EmptyABRC) then gen_etiq() else etL and
                refL=if(!refL=EmptyABRC) then reference else refL in
               refL:=(aux !refL (if etL =0 then etiquettesVisitees else List.append etiquettesVisitees [etL]));
               NodeABRC(x, (refL, etL),(refR, etR))))
           else
             (let etR = if(!refR=EmptyABRC) then gen_etiq() else etR and
               refR=if(!refR=EmptyABRC) then reference else refR in
              refR:=(aux !refR (if etR =0 then etiquettesVisitees else List.append etiquettesVisitees [etR]));
              NodeABRC(x, (refL, etL), (refR, etR))))
        else
          (NodeABRC (insert_ordered_list (v,etiquettesVisitees) x, (refL,etL),(refR,etR)))
    in aux abrc [];;

(*Insertion d'un ensemble des valeurs dans ABRC (avec la creation des etiquettes)*)
let liste_to_abrc_etiq l abrc =
  let rec aux l abrc=
    match l with
    | [] -> abrc
    | x::q ->aux q (insert_abrc_etiq (fst x) (snd x) abrc)
  in aux l abrc

(*Compression d'un ABR*)
let compresse_abr_listes abr =
  (*Avec l'ABR du ennoncé:
      listeConstr1 = [(4, "((())())((())())()"); (2, "(())()"); (1, "()"); (8, "((())())()")]
      listeConstr2 = [(3, 1); (6, 2); (5, 1); (7, 1); (9, 1)]*)
  let (listeConstr1, listeConstr2) = listes_construction abr
  in
  (*Avec l'ABR du ennoncé:       (x [etiquetes]) - noeud
      abrc_init=
                    (4 [])
            (2 [])        (8 [])
      (1 [])                                *)
  let abrc_init = liste_to_abrc (List.map fst listeConstr1)
  in
  (*Avec l'ABR du ennoncé:
      liste_refernces = [(3, reference de (1 [])); (6, reference de (2 [])); (5, reference de (1 []));
                            (7, reference de (1 [])); (9, reference de (1 []))]*)
  let list_references = List.combine (List.map fst listeConstr2) (liste_refs (List.map snd listeConstr2) abrc_init) in
  (*Avec l'ABR du ennoncé:
      abrc_fin=
                                                              (4 [])
                                          (2 []; 6 [2])                         (8 [])
       (1 []; 3 [1]; 5 [2]; 7 [2,1]; 9 [3])            etiq1              etiq2        etiq3
  *)
  let abrc_fin =  liste_to_abrc_etiq list_references abrc_init in
  abrc_fin;;

let arbre_compress = compresse_abr_listes (liste_to_arbre [4;2;8;1;3;6;5;9;7])

let rec liste_egaux l1 l2 = match l1,l2 with
  | ([],[]) -> true
  | ([],x::q) -> false
  | (x::q,[]) -> false
  | (x1::q1, x2::q2) -> if (x1=x2) then liste_egaux q1 q2 else false;;

(*Recherche d'un element dans ABRC. Retourne true or false*)
let rec recherche v arb =
  let rec recherche_liste v lst lst_arretes_rouges= (*compare v aux valeurs dans le noeud, en tenant compte des etiq.*)
    match lst with
    |[]->raise Not_found
    |h::t when liste_egaux (snd h) lst_arretes_rouges ->
      if (fst h)=v then 0        (*egal*)
      else if (fst h)>v  then -1 (*elt avec ces etiquettes est plus petit que v*)
      else 1                     (*elt avec ces etiquettes est plus grand que v*)
    |h::t->recherche_liste v t lst_arretes_rouges;
  in
  let rec aux_rech v arb lst_arretes_rouges =
    let compare v x = recherche_liste v x lst_arretes_rouges in
    match arb with
    | EmptyABRC -> false
    | NodeABRC(x,(refL,etL),(refR,etR)) when compare v x =0->true (*trouve*)
    | NodeABRC(x,(refL,etL),(refR,etR)) when compare v x=(-1) ->  (*descendre a gauche *)
      aux_rech v (!refL) (lst_arretes_rouges@ (if etL=0 then [] else [etL]))
    | NodeABRC(x,(refL,etL),(refR,etR)) when compare v x=1->      (*descendre a droite *)
      aux_rech v (!refR) (lst_arretes_rouges@ (if etR=0 then [] else [etR]))
    | NodeABRC(_,_,_) -> failwith "Should not occure"
  in aux_rech v arb [];;

recherche 6 arbre_compress;;
