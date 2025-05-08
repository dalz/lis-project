open PPrint

let pp_bop e1 op e2 = group (align (e1 ^/^ utf8string op ^^ space ^^ e2))

let pp_assoc_bop aux prec t p1 op_str p2 =
  let p = pp_bop (aux (prec - 1) p1) op_str (aux (prec - 1) p2) in
  if t >= prec then parens p else p
