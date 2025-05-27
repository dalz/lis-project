{ x = 5 * (x % 2) = 1} # idk if this precondition works as intended in the slides
x := alloc()
# { x = 5 * x ↦ _ } TODO: the output doesn't match this postcondition (i guess). is there some error?