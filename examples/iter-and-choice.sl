{ ⊤ }

x := 0;
y := 1;
(y := y + 1)⋆;
(x := x - y)⋆;
((x > 0)?; z := 1) + ((x < 0)?; z := 2);