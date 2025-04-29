open PPrint

let pp_bop e1 op e2 = group (align (e1 ^/^ utf8string op ^^ space ^^ e2))
