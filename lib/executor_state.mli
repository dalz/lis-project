open Base

type heapval = Val of Aexp.t | Undefined | Dealloc

type t = {
  dummies : (Ide.t, Dummy.t, Ide.comparator_witness) Map.t;
  heap : (Dummy.t, heapval, Dummy.comparator_witness) Map.t;
  path_cond : Path_cond.t;
}

val add_bexp_to_path_cond : t -> Bexp.t -> t list
val list_of_norm_prop : Norm_prop.t -> t list

val dummify_aexp :
  (Ide.t, Dummy.t, Ide.comparator_witness) Map.t ->
  Aexp.t ->
  (Ide.t, Dummy.t, Ide.comparator_witness) Map.t * Aexp.t

val dummify_bexp :
  (Ide.t, Dummy.t, Ide.comparator_witness) Map.t ->
  Bexp.t ->
  (Ide.t, Dummy.t, Ide.comparator_witness) Map.t * Bexp.t

val subst : t -> Dummy.t -> Dummy.t -> t option
val simpl : t -> t option
val to_prop : t -> Prop.t
val pretty : t -> PPrint.document
