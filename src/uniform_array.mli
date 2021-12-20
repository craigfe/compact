(** Same semantics as ['a Array.t], except it's guaranteed that the
    representation array is not tagged with [Double_array_tag], the tag for
    float arrays.

    This means it's safer to use in the presence of [Obj.magic], but it's slower
    than normal [Array] if you use it with floats. It can often be faster than
    [Array] if you use it with non-floats. *)

include Uniform_array_intf.Intf
(** @inline *)
