{ y ↦ v }

x ← alloc();
free(y);
x ← [y];

# expected output: 
# [err: x ↦ _ ∗ y ↦̸]