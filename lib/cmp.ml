type t = Le | Lt | Eq | Ne [@@deriving show, equal]

let to_string = function Le -> "≤" | Lt -> "<" | Eq -> "=" | Ne -> "≠"
let compute = function Le -> ( <= ) | Lt -> ( < ) | Eq -> ( = ) | Ne -> ( <> )
