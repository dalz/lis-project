open Norm_prop

(*Check z3 arithmetic rules for deeper simplification which is already implementation*)
(*Evaluate if its better to use  Z3 or reeimplement it *)
let rec simplify_a (a : Aexp.t) : Aexp.t =
  match a with
  | Num n -> Num n
  | Var t -> Var t
  | Bop (a_op, a1, a2) -> (
      let s_a1 = simplify_a a1 in
      let s_a2 = simplify_a a2 in
      match (s_a1, s_a2) with
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

let rec simplify_b (b : Bexp.t) : Bexp.t =
  match b with
  | Const b1 -> Const b1
  | Not b1 -> (
      match b1 with
      | Bop (op, Not bexp1, Not bexp2) -> (
          match op with
          | And -> Bop (Or, simplify_b bexp1, simplify_b bexp2)
          | Or -> Bop (And, simplify_b bexp1, simplify_b bexp2))
      | Bop (op, bexp1, bexp2) -> (
          match op with
          | And -> Bop (Or, simplify_b (Not bexp1), simplify_b (Not bexp2))
          | Or -> Bop (And, simplify_b (Not bexp1), simplify_b (Not bexp2)))
      | Const b -> ( match b with true -> Const false | false -> Const true)
      | Not b -> simplify_b b
      | _ -> Not (simplify_b b1))
  | Bop (op, bexp1, bexp2) -> (
      let eval1 = simplify_b bexp1 in
      let eval2 = simplify_b bexp2 in
      match op with
      | And -> (
          match eval1 with
          | Const false -> Const false
          | Const true -> eval2
          | _ -> (
              match eval2 with
              | Const false -> Const false
              | Const true -> eval1
              | _ -> Bop (And, eval1, eval2)))
      | Or -> (
          match eval1 with
          | Const true -> Const true
          | Const false -> eval2
          | _ -> (
              match eval2 with
              | Const true -> Const true
              | Const false -> eval1
              | _ -> Bop (Or, eval1, eval2))))
  | Cmp (op, aexp1, aexp2) -> (
      let eval1 = simplify_a aexp1 in
      let eval2 = simplify_a aexp2 in
      match op with
      | Le -> (
          match eval1 with
          | Num n1 -> (
              match eval2 with
              | Num n2 -> Const (n1 <= n2)
              | _ -> Cmp (Le, eval1, eval2))
          | _ -> Cmp (Le, eval1, eval2))
      | Lt -> (
          match eval1 with
          | Num n1 -> (
              match eval2 with
              | Num n2 -> Const (n1 < n2)
              | _ -> Cmp (Lt, eval1, eval2))
          | _ -> Cmp (Lt, eval1, eval2))
      | Eq -> (
          match eval1 with
          | Num n1 -> (
              match eval2 with
              | Num n2 ->
                  Const (n1 = n2) (* Use single '=' for structural equality *)
              | _ -> Cmp (Eq, eval1, eval2))
          | Var v1 -> (
              match eval2 with
              | Var v2 ->
                  Const (v1 = v2) (* Use single '=' for structural equality *)
              | _ -> Cmp (Eq, eval1, eval2))
          | _ -> Cmp (Eq, eval1, eval2)))

let simplify_atom (at : Atom.t) : Atom.t =
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

(* Function that returns false if x -> w * x -> v, by scanning through all the atoms *)
(* TODO use a set instead of list, this is highly inefficient with lists, how should we handle this?*)
(* let has_no_duplicates lst =
  let rec aux seen = function
    | [] -> true
    | x :: xs ->
        if List.exists (fun y -> y = x) seen then false else aux (x :: seen) xs
  in
  aux [] lst *)

let rec move_points_num_first (atoms_list : Atom.t list) : Atom.t list =
  match atoms_list with
  | Atom.PointsTo (x, Num n) :: rest ->
      Atom.PointsTo (x, Num n) :: move_points_num_first rest
  | a :: Atom.PointsTo (x, Num n) :: rest ->
      Atom.PointsTo (x, Num n) :: a :: move_points_num_first rest
  | a :: b :: rest -> move_points_num_first rest @ [ a; b ]
  | _ -> atoms_list

let populate_hash (hash : (Dummy.t, Aexp.t list) Hashtbl.t)
    (atoms : Atom.t list) : Atom.t list =
  List.filter
    (fun atom ->
      match atom with
      | Atom.PointsTo (x, a) ->
          if Hashtbl.mem hash x then (
            Hashtbl.replace hash x (a :: Hashtbl.find hash x);
            false)
          else (
            Hashtbl.add hash x [ a ];
            true)
      | _ -> true)
    atoms

let create_equalities (aexps : Aexp.t list) : Atom.t list =
  match aexps with
  | [] -> []
  | _ ->
      let first = List.nth aexps 0 in
      List.fold_right
        (fun aexp (eqs : Atom.t list) ->
          Bool (Bexp.Cmp (Eq, first, aexp)) :: eqs)
        aexps []

let add_equalities (atoms_list : Atom.t list) : Atom.t list =
  let seen_vars = Hashtbl.create 16 in
  let num_first = move_points_num_first atoms_list in
  let no_dups = populate_hash seen_vars num_first in
  Hashtbl.fold
    (fun _ value prev_atoms -> create_equalities value @ prev_atoms)
    seen_vars no_dups

let simplify_conj (atoms : conj) : conj =
  (*discuss parametric impl*)
  (* Extract atoms list from conj *)
  let atoms_list = match atoms with Conj a -> a in
  (* Removes multiple points to refs and adds needed equalities*)
  let list_with_eq = add_equalities atoms_list in
  (* Simplify atoms *)
  let simpl_list = List.map (function at -> simplify_atom at) list_with_eq in
  (* Remove AND true *)
  let filt_list =
    List.filter
      (function
        | at -> (
            match at with Atom.Bool (Bexp.Const true) -> false | _ -> true))
      simpl_list
  in
  (* Remove duplicates // this is false*)
  let unique_list = remove_from_the_right filt_list in
  (* Checks if there is False *)
  if
    List.exists
      (function
        | at -> (
            match at with Atom.Bool (Bexp.Const false) -> true | _ -> false))
      unique_list
  then Conj [ Bool (Bexp.Const false) ]
  else Conj unique_list

(* TODO check if x -> v and x -> w / v!=w *)
(* Acumulate via qualitative constraint, and evaluate later in the program the results*)

(* Returns true iff the given sepj has at least two PointsTo containing the same variable.
  This function helps to implement this simplification rule: x -> v * x -> w = false *)
let sepj_has_conflict (Sepj conj_list) : bool =
  let seen_vars = Hashtbl.create 16 in

  (* Checks if any PointsTo in a conj references a variable already seen. *)
  let rec check_atoms = function
    | [] -> false
    | Atom.PointsTo (var_name, _) :: tail ->
        if Hashtbl.mem seen_vars var_name then true
        else (
          Hashtbl.add seen_vars var_name true;
          check_atoms tail)
    | _ :: tail -> check_atoms tail
  in

  (* Iterates through each conj until a conflict is found. *)
  let rec check_conflicts = function
    | [] -> false
    | Conj atoms :: rest ->
        if check_atoms atoms then true else check_conflicts rest
  in

  check_conflicts conj_list

let simplify_sepj (conjs : sepj) : sepj =
  (* Extract conjs from sepj *)
  let conjs_list = match conjs with Sepj s -> s in

  (* Simplify each conj in the list *)
  let simpl_list =
    List.map (function conj -> simplify_conj conj) conjs_list
  in
  (* Checks if there is False *)
  if
    List.exists
      (function
        | Conj atoms ->
            List.exists
              (function Atom.Bool (Bexp.Const false) -> true | _ -> false)
              atoms)
      simpl_list
  then Sepj [ Conj [ Bool (Bexp.Const false) ] ]
  else
    (* Remove * empt from the list of conjs *)
    let filter_list =
      List.filter (function conj_exp -> conj_exp != Conj [ Emp ]) simpl_list
    in
    (* If there's a conflict (x->v * x->w), the whole sepj becomes false *)
    if sepj_has_conflict (Sepj filter_list) then
      Sepj [ Conj [ Bool (Bexp.Const false) ] ]
    (* Remove duplicates *)
      else
      let (Sepj no_duplicates_list) =
        Sepj (remove_from_the_right filter_list)
      in
      let len = List.length no_duplicates_list in
      Sepj
        (List.filter
           (fun x ->
             match x with
             | Conj l -> (l = [] && len = 1) || (l <> [] && len >= 1))
           no_duplicates_list)

let simply_dsj (sepjs : disj) : disj =
  (* Extract the list of sepjs from disj *)
  let sep_list = match sepjs with Disj d -> d in
  (* Simplify each sepj inside the list *)
  let simpl_list = List.map (function sep -> simplify_sepj sep) sep_list in
  (* Removign OR false *)
  (*List.filter
      (function at -> at != Sepj [ Conj [ Bool (Bexp.Const false) ] ])
      simpl_list*)

  let filt_list =
    List.filter
      (function
        | at -> (
            match at with
            | Sepj [ Conj [ Bool (Bexp.Const false) ] ] -> false
            | _ -> true))
      simpl_list
  in
  (* Removing duplicates *)
  let unique_list = remove_from_the_right filt_list in
  if
    (* Check if there is OR true *)
    List.exists
      (function
        | at -> (
            match at with
            | Sepj [ Conj [ Bool (Bexp.Const true) ] ] -> true
            | _ -> false))
      unique_list
  then Disj [ Sepj [ Conj [ Bool (Bexp.Const true) ] ] ]
  else Disj unique_list

let simplify_t (proposition : t) : t =
  match proposition with
  | id_list, disjunction -> (id_list, simply_dsj disjunction)

let rec get_identifiers (expr : Prop.t) =
  let result = [] in
  match expr with
  | Or (p1, p2) ->
      let result = get_identifiers p1 @ get_identifiers p2 in
      result
  | And (p1, p2) ->
      let result = get_identifiers p1 @ get_identifiers p2 in
      result
  | Exists (iden, p) ->
      let result1 = get_identifiers p in
      iden :: result1
  | Sep (p1, p2) ->
      let result = get_identifiers p1 @ get_identifiers p2 in
      result
  | Atom a -> (
      match a with
      | Bool _ -> result
      | Emp -> result
      | PointsTo (ident, _) -> ident :: result
      | PointsToNothing ident -> ident :: result)

(* TODO eventually add exist simplification for dummy variables *)
let rec simplify_prop (expr : Prop.t) : Prop.t =
  match expr with
  | Or (prop1, prop2) -> (
      let eval1 = simplify_prop prop1 in
      let eval2 = simplify_prop prop2 in
      match eval1 with
      | Atom (Bool (Bexp.Const true)) -> Atom (Bool (Bexp.Const true))
      | Atom (Bool (Bexp.Const false)) -> eval2
      | _ -> (
          match eval2 with
          | Atom (Bool (Bexp.Const true)) -> Atom (Bool (Bexp.Const true))
          | Atom (Bool (Bexp.Const false)) -> eval1
          | _ -> Or (eval1, eval2)))
  | And (prop1, prop2) -> (
      let eval1 = simplify_prop prop1 in
      let eval2 = simplify_prop prop2 in
      match eval1 with
      | Atom (Bool (Bexp.Const false)) -> Atom (Bool (Bexp.Const false))
      | Atom (Bool (Bexp.Const true)) -> eval2
      | _ -> (
          match eval2 with
          | Atom (Bool (Bexp.Const false)) -> Atom (Bool (Bexp.Const false))
          | Atom (Bool (Bexp.Const true)) -> eval1
          | _ -> And (eval1, eval2)))
  | Exists (iden, p) -> (
      match simplify_prop p with
      | Atom (Bool (Bexp.Const true)) -> Atom (Bool (Bexp.Const true))
      | Atom (Bool (Bexp.Const false)) -> Atom (Bool (Bexp.Const false))
      | _ ->
          let simplified_prop = simplify_prop p in
          if List.exists (fun x -> x = iden) (get_identifiers simplified_prop)
          then Exists (iden, simplified_prop)
          else simplified_prop)
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
  | Atom a -> Atom (simplify_atom a)
