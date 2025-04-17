open Ast

let rec simplify_a (a : aexp) : aexp =
  match a with _ -> failwith "not implemented yet"

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

let rec simplify_prop expr : prop =
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
