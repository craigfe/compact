(** An {i immediate} array is an array of elements that {i always} have an
    immediate runtime representation (i.e. [Obj.is_int] always holds).

    This requirement enables an array implementation that can represent
    singleton arrays as immediates (using 1 word rather than 3). A consequence
    of this is that the API is not purely mutable: functions that "mutate" an
    array must return that array (to cover the case of the array being an
    immediate singleton).

    This module is not exposed for external use. *)

type 'a t

val empty : _ t
val create : len:int -> 'a -> 'a t
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val of_list_rev : 'a list -> 'a t
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> f:('acc -> 'a -> 'acc) -> init:'acc -> 'acc
val exists : 'a t -> f:('a -> bool) -> bool
val to_array : 'a t -> 'a array
val length : _ t -> int
val singleton : 'a -> 'a t

val unsafe_blit :
  src:'a t -> src_pos:int -> dst:'a t -> dst_pos:int -> len:int -> 'a t

val unsafe_set : 'a t -> int -> 'a -> 'a t
val unsafe_get : 'a t -> int -> 'a
