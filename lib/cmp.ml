type t = Le | Lt | Eq | Ne [@@deriving show, equal, compare]

let to_string = function Le -> "≤" | Lt -> "<" | Eq -> "=" | Ne -> "≠"
let compute = function Le -> ( <= ) | Lt -> ( < ) | Eq -> ( = ) | Ne -> ( <> )
