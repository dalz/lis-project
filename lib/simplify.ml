open Ast
open Norm_prop

let rec simplify_a (a : aexp) : aexp =
  match a with
  | Num n -> Num n
  | Var t -> Var t
  | Bop (a_op, a1, a2) -> (
      let s_a1 = simplify_a a1 in
      let s_a2 = simplify_a a2 in
      match (s_a1, s_a1) with
      | Num n1, Num n2 -> (
          match a_op with
          | Sum -> Num (n1 + n2)
          | Sub -> Num (n1 - n2)
          | Mul -> Num (n1 * n2)
          | Div -> (
              match n2 with
              | 0 -> failwith "Attempting division by zero"
              (* This rounds if result is not integer *)
              | _ -> Num (n1 / n2))
          | Mod -> Num (n1 mod n2))
      | _ -> Bop (a_op, s_a1, s_a2))
  | Uop (_, n) -> (
      match n with
      | Uop (Neg, a1) -> simplify_a a1
      | _ -> Uop (Neg, simplify_a n))

let rec simplify_b (b : bexp) : bexp =
  match b with
  | BConst b1 -> BConst b1
  | Not b1 -> (
      match b1 with
      | BConst true -> BConst false
      | BConst false -> BConst true
      | _ -> Not (simplify_b b1))
  | BAnd (bexp1, bexp2) -> (
      let eval1 = simplify_b bexp1 in
      let eval2 = simplify_b bexp2 in
      match eval1 with
      | BConst false -> BConst false
      | BConst true -> eval2
      | _ -> (
          match eval2 with
          | BConst false -> BConst false
          | BConst true -> eval1
          | _ -> BAnd (eval1, eval2)))
  | BOr (bexp1, bexp2) -> (
      let eval1 = simplify_b bexp1 in
      let eval2 = simplify_b bexp2 in
      match eval1 with
      | BConst true -> BConst true
      | BConst false -> eval2
      | _ -> (
          match eval2 with
          | BConst true -> BConst true
          | BConst false -> eval1
          | _ -> BOr (eval1, eval2)))
  | Cmp (op, aexp1, aexp2) -> (
      let eval1 = simplify_a aexp1 in
      let eval2 = simplify_a aexp2 in
      match op with
      | Le -> (
          match eval1 with
          | Num n1 -> (
              match eval2 with
              | Num n2 -> BConst (n1 <= n2)
              | _ -> Cmp (Le, eval1, eval2))
          | _ -> Cmp (Le, eval1, eval2))
      | Lt -> (
          match eval1 with
          | Num n1 -> (
              match eval2 with
              | Num n2 -> BConst (n1 < n2)
              | _ -> Cmp (Lt, eval1, eval2))
          | _ -> Cmp (Lt, eval1, eval2))
      | Eq -> (
          match eval1 with
          | Num n1 -> (
              match eval2 with
              | Num n2 -> BConst (n1 == n2)
              | _ -> Cmp (Eq, eval1, eval2))
          | _ -> Cmp (Eq, eval1, eval2)))

let rec simplify_atom (at : atom) : atom =
  match at with
  | Bool b -> Bool (simplify_b b)
  | Emp -> Emp
  | PointsToNothing t -> PointsToNothing t
  | PointsTo (t, a) -> PointsTo (t, simplify_a a)

(* TODO check that this works as expected *)
(* !!! copy and pasted *)
(* Utility to remove duplicates from a list*)
let rec remove_from_the_right l =
  match l with
  | [] -> []
  | h :: t -> h :: remove_from_the_right (List.filter (fun x -> x <> h) t)

let rec simplify_conj (atoms : conj) : conj =
  (* Extract atoms list from conj *)
  let atoms_list = match atoms with Conj a -> a in
  (* Simplify atoms *)
  let simpl_list = List.map (function at -> simplify_atom at) atoms_list in
  (* Remove AND true *)
  let filt_list =
    (* TODO check if this filter works as expected *)
    List.filter (function at -> at != Bool (BConst true)) simpl_list
  in
  (* Remove duplicates *)
  let unique_list = remove_from_the_right filt_list in
  (* Checks if there is False *)
  if List.exists (function at -> at == Bool (BConst false)) unique_list then
    Conj [ Bool (BConst false) ]
  else Conj unique_list
(* TODO check if x -> v and x -> w / v!=w *)

let rec simplify_sepj (conjs : sepj) : sepj =
  (* Extract conjs from sepj *)
  let conjs_list = match conjs with Sepj s -> s in

  (* Simplify each conj in the list *)
  let simpl_list =
    List.map (function conj -> simplify_conj conj) conjs_list
  in
  (* Remove * empt from the list of conjs *)
  let filter_list =
    List.filter (function conj_exp -> conj_exp != Conj [ Emp ]) simpl_list
  in
  (* Remove duplicates *)
  let unique_list = remove_from_the_right filter_list in
  Sepj unique_list
(* TODO, for each varaible check x -> v * x -> w = false *)

let rec simply_dsj (sepjs : disj) : disj =
  (* Extract the list of sepjs from disj *)
  let sep_list = match sepjs with Disj d -> d in
  (* Simplify each sepj inside the list *)
  let simpl_list = List.map (function sep -> simplify_sepj sep) sep_list in
  (* Removign OR false *)
  let filt_list =
    List.filter
      (function at -> at != Sepj [ Conj [ Bool (BConst false) ] ])
      simpl_list
  in
  (* Removing duplicates *)
  let unique_list = remove_from_the_right filt_list in
  if
    (* Check if there is OR true *)
    List.exists
      (function at -> at == Sepj [ Conj [ Bool (BConst true) ] ])
      unique_list
  then Disj [ Sepj [ Conj [ Bool (BConst true) ] ] ]
  else Disj unique_list

let simplify_t (proposition : t) : t =
  match proposition with
  | id_list, disjunction -> (id_list, simply_dsj disjunction)

(* TODO eventually remove simplify prop *)
let rec simplify_prop expr =
  match expr with
  | Or (prop1, prop2) -> (
      let eval1 = simplify_prop prop1 in
      let eval2 = simplify_prop prop2 in
      match eval1 with
      | Atom (Bool (BConst true)) -> Atom (Bool (BConst true))
      | Atom (Bool (BConst false)) -> eval2
      | _ -> (
          match eval2 with
          | Atom (Bool (BConst true)) -> Atom (Bool (BConst true))
          | Atom (Bool (BConst false)) -> eval1
          | _ -> Or (eval1, eval2)))
  | And (prop1, prop2) -> (
      let eval1 = simplify_prop prop1 in
      let eval2 = simplify_prop prop2 in
      match eval1 with
      | Atom (Bool (BConst false)) -> Atom (Bool (BConst false))
      | Atom (Bool (BConst true)) -> eval2
      | _ -> (
          match eval2 with
          | Atom (Bool (BConst false)) -> Atom (Bool (BConst false))
          | Atom (Bool (BConst true)) -> eval1
          | _ -> And (eval1, eval2)))
  | Exists (iden, p) -> (
      match simplify_prop p with
      | Atom (Bool (BConst true)) -> Atom (Bool (BConst true))
      | Atom (Bool (BConst false)) -> Atom (Bool (BConst false))
      | _ -> Exists (iden, simplify_prop p))
  | Sep (p1, p2) -> (
      let eval1 = simplify_prop p1 in
      let eval2 = simplify_prop p2 in
      if eval1 == eval2 then eval2
      else
        match eval1 with
        | Atom Emp -> eval2
        | _ -> ( match eval2 with Atom Emp -> eval1 | _ -> Sep (eval1, eval2))
      (* TODO add simplification for x -> 5 * x -> 7 this should be simplified to false *)
      )
  | Atom (Bool b) -> Atom (Bool (simplify_b b))
  | _ -> failwith ""
