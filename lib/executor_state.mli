open Base

type heapval = Val of Aexp.t | Undefined | Dealloc

type t = {
  exvars : Dummy.t list;
  dummies : (Ide.t, Dummy.t, Ide.comparator_witness) Map.t;
  heap : (Dummy.t, heapval, Dummy.comparator_witness) Map.t;
  path_cond : Bexp.t;
}

val list_of_norm_prop : Norm_prop.t -> t list

val dummify_aexp :
  (Ide.t, Dummy.t, Ide.comparator_witness) Map.t ->
  Aexp.t ->
  (Ide.t, Dummy.t, Ide.comparator_witness) Map.t * Aexp.t

val dummify_bexp :
  (Ide.t, Dummy.t, Ide.comparator_witness) Map.t ->
  Bexp.t ->
  (Ide.t, Dummy.t, Ide.comparator_witness) Map.t * Bexp.t

val pretty : t -> PPrint.document
