(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2016–2020 Jane Street Group, LLC <opensource@janestreet.com>
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

module type S = sig
  type ('a, 'b, 'c) t
  type ('a, 'b, 'c) elt
  type ('a, 'b, 'c, 'inner) with_elt

  val empty : (_, _, _) t
  val create : len:int -> ('a, 'b, 'c, ('a, 'b, 'c) t) with_elt
  val singleton : ('a, 'b, 'c, ('a, 'b, 'c) t) with_elt
  val init : int -> f:(int -> ('a, 'b, 'c) elt) -> ('a, 'b, 'c) t
  val length : (_, _, _) t -> int
  val get : ('a, 'b, 'c) t -> int -> ('a, 'b, 'c) elt
  val unsafe_get : ('a, 'b, 'c) t -> int -> ('a, 'b, 'c) elt
  val set : ('a, 'b, 'c) t -> int -> ('a, 'b, 'c, unit) with_elt
  val unsafe_set : ('a, 'b, 'c) t -> int -> ('a, 'b, 'c, unit) with_elt
  val swap : (_, _, _) t -> int -> int -> unit

  val map :
       ('a1, 'b1, 'c1) t
    -> f:('a1, 'b1, 'c1, ('a2, 'b2, 'c2) elt) with_elt
    -> ('a2, 'b2, 'c2) t

  val map_inplace :
    ('a, 'b, 'c) t -> f:('a, 'b, 'c, ('a, 'b, 'c) elt) with_elt -> unit

  val map2_exn :
       ('a1, 'b1, 'c1) t
    -> ('a2, 'b2, 'c2) t
    -> f:('a1, 'b1, 'c1, ('a2, 'b2, 'c2, ('a3, 'b3, 'c3) elt) with_elt) with_elt
    -> ('a3, 'b3, 'c3) t
  (** Functions with the 2 suffix raise an exception if the lengths of the two
      given arrays aren't the same. *)

  val iter : ('a, 'b, 'c) t -> f:('a, 'b, 'c, unit) with_elt -> unit

  val iteri : ('a, 'b, 'c) t -> f:(int -> ('a, 'b, 'c, unit) with_elt) -> unit
  (** Like {!iter}, but the function is applied to the index of the element as
      first argument, and the element itself as second argument. *)

  val fold :
       ('a, 'b, 'c) t
    -> init:'acc
    -> f:('acc -> ('a, 'b, 'c, 'acc) with_elt)
    -> 'acc

  val exists : ('a, 'b, 'c) t -> f:('a, 'b, 'c, bool) with_elt -> bool

  val of_array : ('a, 'b, 'c) elt array -> ('a, 'b, 'c) t
  (** [of_array] and [to_array] return fresh arrays with the same contents
      rather than returning a reference to the underlying array. *)

  val to_array : ('a, 'b, 'c) t -> ('a, 'b, 'c) elt array
  val of_list : ('a, 'b, 'c) elt list -> ('a, 'b, 'c) t
  val of_list_rev : ('a, 'b, 'c) elt list -> ('a, 'b, 'c) t
  val to_list : ('a, 'b, 'c) t -> ('a, 'b, 'c) elt list

  (** {2 Extra lowlevel and unsafe functions} *)

  val unsafe_blit :
       src:('a, 'b, 'c) t
    -> src_pos:int
    -> dst:('a, 'b, 'c) t
    -> dst_pos:int
    -> len:int
    -> unit

  val unsafe_create_uninitialized : len:int -> (_, _, _) t
  (** The behavior is undefined if you access an element before setting it. *)

  val create_obj_array : len:int -> (Obj.t, Obj.t, Obj.t) t
  (** New obj array filled with [Obj.repr 0] *)

  val unsafe_clear_if_pointer : (Obj.t, Obj.t, Obj.t) t -> int -> unit
  (** [unsafe_clear_if_pointer t i] prevents [t.(i)] from pointing to anything
      to prevent space leaks. It does this by setting [t.(i)] to
      [Caml.Obj.repr 0]. As a performance hack, it only does this when
      [not (Caml.Obj.is_int t.(i))]. It is an error to access the cleared index
      before setting it again. *)
end

module type Intf = sig
  module type S = S

  type 'a t

  (** @inline *)
  include
    S
      with type ('a, _, _) t := 'a t
       and type ('a, _, _) elt := 'a
       and type ('a, _, _, 'inner) with_elt := 'a -> 'inner

  val unsafe_set_omit_phys_equal_check : 'a t -> int -> 'a -> unit
  (** [unsafe_set_omit_phys_equal_check] is like [unsafe_set], except it doesn't
      do a [phys_equal] check to try to skip [caml_modify]. It is safe to call
      this even if the values are [phys_equal]. *)

  val unsafe_set_assuming_currently_int : Obj.t t -> int -> Obj.t -> unit
  (** [unsafe_set_assuming_currently_int t i obj] sets index [i] of [t] to
      [obj], but only works correctly if the value there is an immediate, i.e.
      [Obj.is_int (get t i)]. This precondition saves a dynamic check.

      [unsafe_set_int_assuming_currently_int] is similar, except the value being
      set is an int.

      [unsafe_set_int] is similar but does not assume anything about the target. *)

  val unsafe_set_int_assuming_currently_int : Obj.t t -> int -> int -> unit
  val unsafe_set_int : Obj.t t -> int -> int -> unit

  include Invariant.S1 with type 'a t := 'a t

  (** {2 Arrays of tuples}

      Compact uniform arrays of tuples: e.g. an [('a, 'b) Tuple2.t] is
      equivalent to [('a * 'b) t] but has a more compact memory representation.

      Each tuple array has a memory overhead of {b 2 words}: a 1-word pointer to
      a heap-allocated block with a 1-word header, with every other word storing
      data (just as for a regular array). This is slightly more efficient than
      just storing a tuple of arrays (which has [(2 + 2n)]-words of overhead for
      a tuple of arity [n]). This is useful for datastructures that may contain
      many small tuple arrays (such as the buckets of a hashtable). *)

  module Tuple2 : sig
    type ('a, 'b) t

    (** @inline *)
    include
      S
        with type ('a, 'b, _) t := ('a, 'b) t
         and type ('a, 'b, _) elt := 'a * 'b
         and type ('a, 'b, _, 'inner) with_elt := 'a -> 'b -> 'inner

    val get_fst : ('a, _) t -> int -> 'a
    val get_snd : (_, 'b) t -> int -> 'b
    val set_fst : ('a, _) t -> int -> 'a -> unit
    val set_snd : (_, 'b) t -> int -> 'b -> unit
    val unsafe_get_fst : ('a, _) t -> int -> 'a
    val unsafe_get_snd : (_, 'b) t -> int -> 'b
    val unsafe_set_fst : ('a, _) t -> int -> 'a -> unit
    val unsafe_set_snd : (_, 'b) t -> int -> 'b -> unit
    val invariant : ('a * 'b) Invariant.t -> ('a, 'b) t Invariant.t
  end

  module Tuple3 : sig
    type ('a, 'b, 'c) t

    (** @inline *)
    include
      S
        with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
         and type ('a, 'b, 'c) elt := 'a * 'b * 'c
         and type ('a, 'b, 'c, 'inner) with_elt := 'a -> 'b -> 'c -> 'inner

    val get_fst : ('a, _, _) t -> int -> 'a
    val get_snd : (_, 'b, _) t -> int -> 'b
    val get_thd : (_, _, 'c) t -> int -> 'c
    val set_fst : ('a, _, _) t -> int -> 'a -> unit
    val set_snd : (_, 'b, _) t -> int -> 'b -> unit
    val set_thd : (_, _, 'c) t -> int -> 'c -> unit
    val unsafe_get_fst : ('a, _, _) t -> int -> 'a
    val unsafe_get_snd : (_, 'b, _) t -> int -> 'b
    val unsafe_get_thd : (_, _, 'c) t -> int -> 'c
    val unsafe_set_fst : ('a, _, _) t -> int -> 'a -> unit
    val unsafe_set_snd : (_, 'b, _) t -> int -> 'b -> unit
    val unsafe_set_thd : (_, _, 'c) t -> int -> 'c -> unit
    val invariant : ('a * 'b * 'c) Invariant.t -> ('a, 'b, 'c) t Invariant.t
  end
end

(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2016–2020 Jane Street Group, LLC <opensource@janestreet.com>
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
  ————————————————————————————————————————————————————————————————————————————*)
