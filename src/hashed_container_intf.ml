(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

module type S = sig
  type ('key, 'value, 'kv_packed, 'decoder, 'kv_pair) t
  (** The type of hashtables with externally-stored bindings. The type
      parameters are as follows:

      - ['key]: the type of {i keys} in the hashtable;

      - ['value]: the type of {i values} in the hashtable;

      - ['kv_packed]: the type of {i packed binding references}. A ['kv_packed]
        is conceptually a compact reference to a key / value pair that may be
        dereferenced with a ['decoder]. The size of the runtime representation
        of _this_ type will determine the

      - ['decoder]: some external state necessary to unpack the internal
        bindings before reading them.

      - ['kv_pair]: the type of key / value pairs post decoding.

      Example types:

      {[
        type 'a hashset       = ('a, unit, 'a, unit, 'a)
        type ('k, 'v) hashtbl = ('k, 'v, 'k * 'v, unit, 'k * 'v)
        type ('k, 'v) ext_tbl = ('k, 'v, file_offset, io_reader, 'k * 'v)
      ]} *)

  type 'self key
  type ('inner, 'k, 'v, 'kv_packed) with_internal_entry
  type ('inner, 'k, 'v, 'kv_entry) with_external_entry
  type ('k, 'v, 'kv_entry) external_entry
  type ('inner, 'd) with_decoder

  val remove : ('k, _, _, 'd, _) t -> ('k key -> unit, 'd) with_decoder
  val mem : ('k, _, _, 'd, _) t -> ('k key -> bool, 'd) with_decoder

  val cardinal : (_, _, _, _, _) t -> int
  (** [cardinal t] is the number of bindings in [t]. *)

  val clear : (_, _, _, _, _) t -> unit
  (** [clear t] removes all bindings from [t]. *)

  (** {2 Iterators} *)

  val iter :
       ('k, 'v, _, 'd, 'e) t
    -> (f:(unit, 'k, 'v, 'e) with_external_entry -> unit, 'd) with_decoder

  val fold :
       ('k, 'v, _, 'd, 'e) t
    -> ( f:('acc -> ('acc, 'k, 'v, 'e) with_external_entry) -> init:'acc -> 'acc
       , 'd )
       with_decoder

  val for_all :
       ('k, 'v, _, 'd, 'e) t
    -> (f:(bool, 'k, 'v, 'e) with_external_entry -> bool, 'd) with_decoder

  val exists :
       ('k, 'v, _, 'd, 'e) t
    -> (f:(bool, 'k, 'v, 'e) with_external_entry -> bool, 'd) with_decoder

  val count :
       ('k, 'v, _, 'd, 'e) t
    -> (f:(bool, 'k, 'v, 'e) with_external_entry -> int, 'd) with_decoder

  val to_list :
    ('k, 'v, _, 'd, 'e) t -> (('k, 'v, 'e) external_entry list, 'd) with_decoder

  val to_sorted_seq :
       ('k, 'v, _, 'd, 'e) t
    -> (('k, 'v, 'e) external_entry Seq.t, 'd) with_decoder

  val copy : ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t

  (** {2:bucket Buckets}

      The container consists of an array of {i buckets}, each containing the
      subset of entries that share a particular hash prefix. For a well-chosen
      hash function, the number of elements in each bucket should be very small
      (usually 0 or 1).

      Most users should not interact with the container buckets directly (and
      use the higher-level API above instead). *)

  val bucket_count : (_, _, _, _, _) t -> int
  (** Returns the number of {i buckets} in the container. A bucket is a slot in
      the container's internal hashtable, to which elements are assigned based
      on the hash value of their key. *)

  (** {2:hash-policy Hash policy} *)

  val load_factor : (_, _, _, _, _) t -> float
  (** Returns the average number of elements per bucket. That is:
      [cardinal /
      bucket_count]. Complexity: O(1). *)

  val reserve : (_, _, _, _, _) t -> int -> unit
  (** Reserves space for at least the specified number of elements and
      regenerates the hash table. *)
end

module type Set = sig
  type 'a t
  type 'a key

  val add : 'a t -> 'a key -> unit
  val map : 'a t -> f:('a key -> 'a key) -> 'a t
  val map_inplace : 'a t -> f:('a key -> 'a key) -> unit

  (** @inline *)
  include
    S
      with type ('a, _, _, _, _) t := 'a t
       and type 'a key := 'a key
       and type ('a, _, _) external_entry := 'a key
       and type ('inner, 'a, _, _) with_external_entry := 'a key -> 'inner
       and type ('inner, 'a, _, _) with_internal_entry := 'a key -> 'inner
       and type ('inner, _) with_decoder := 'inner
end

module type Assoc = sig
  type ('a, 'b, 'c, 'd, 'e) t
  type ('a, 'b) with_decoder
  type ('a, 'b, 'c, 'd) with_internal_entry

  val replace :
       ('k, 'v, 'kv_p, 'd, _) t
    -> ((unit, 'k, 'v, 'kv_p) with_internal_entry, 'd) with_decoder
  (** Adds or replaces an existing binding. *)

  type ok_or_duplicate := [ `Ok | `Duplicate ]

  val add :
       ('k, 'v, 'kv_p, 'd, _) t
    -> ((ok_or_duplicate, 'k, 'v, 'kv_p) with_internal_entry, 'd) with_decoder

  val add_exn :
       ('k, 'v, 'kv_p, 'd, _) t
    -> ((unit, 'k, 'v, 'kv_p) with_internal_entry, 'd) with_decoder

  val find : ('k, 'v, _, 'd, _) t -> ('k -> 'v option, 'd) with_decoder
  (** [find t k] is [Some v] if [k] is bound to [v] in [t], or [None] if no such
      binding exists. *)

  val find_and_call :
       ('k, 'v, _, 'd, _) t
    -> (    'k
         -> if_found:(key:'k -> data:'v -> 'r)
         -> if_not_found:('k -> 'r)
         -> 'r
       , 'd )
       with_decoder

  val find_exn : ('k, 'v, _, 'd, _) t -> ('k -> 'v, 'd) with_decoder
  (** [find_exn t d k] is the value to which [k] is bound in [t], if it exists.

      @raise Not_found if there is no binding for [k] in [t]. *)

  val iter_keys :
    ('k, _, _, 'd, _) t -> (f:('k -> unit) -> unit, 'd) with_decoder

  include
    S
      with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t
       and type ('a, 'b) with_decoder := ('a, 'b) with_decoder
       and type ('a, 'b, 'c, 'd) with_internal_entry :=
        ('a, 'b, 'c, 'd) with_internal_entry

  val invariant :
    ( 'k Invariant.t -> 'v Invariant.t -> ('k, 'v, _, 'd, _) t Invariant.t
    , 'd )
    with_decoder
end

module type Types = sig
  type ('key, 'value, 'packed, 'decoder, 'entry) vtable =
    { key_hash : 'key -> int
    ; key_hash_size : int
    ; key_equal : 'key -> 'key -> bool
    ; entry_key : 'entry -> 'key
    ; entry_value : 'entry -> 'value
    ; entry_compare : 'entry -> 'entry -> int
    ; packed_key : 'decoder -> 'packed -> 'key
    ; packed_entry : 'decoder -> 'packed -> 'entry
    ; packed_of_entry : 'decoder -> 'entry -> 'packed
    }
end

module rec Types : Types = Types

module type Intf = sig
  module type S = S
  module type Set = Set
  module type Assoc = Assoc

  include
    Assoc
      with type 'self key := 'self
       and type ('inner, 'key, _, 'kv_packed) with_internal_entry :=
        'key -> 'kv_packed -> 'inner
       and type (_, _, 'entry) external_entry := 'entry
       and type ('inner, _, _, 'entry) with_external_entry := 'entry -> 'inner
       and type ('inner, 'd) with_decoder := decoder:'d -> 'inner

  include Types

  val map :
       ('k, 'v, 'kv_p, 'd, 'e) t
    -> decoder_src:'d
    -> decoder_dst:'d
    -> f:('e -> 'e)
    -> ('k, 'v, 'kv_p, 'd, 'e) t

  val map_inplace :
       ('k, 'v, 'kv_p, 'd, 'e) t
    -> decoder_src:'d
    -> decoder_dst:'d
    -> f:('e -> 'e)
    -> unit

  val vtable : ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) vtable

  val map_poly :
       ('k, 'v1, 'kv_p1, 'd1, 'e1) t
    -> vtable:('k, 'v2, 'kv_p2, 'd2, 'e2) vtable
    -> decoder_src:'d1
    -> decoder_dst:'d2
    -> f:('e1 -> 'e2)
    -> ('k, 'v2, 'kv_p2, 'd2, 'e2) t

  module No_decoder : sig
    type nonrec ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, unit, 'd) t

    include
      Assoc
        with type ('a, 'b, 'c, _, 'e) t := ('a, 'b, 'c, 'e) t
         and type 'self key := 'self
         and type ('inner, 'key, _, 'kv_packed) with_internal_entry :=
          'key -> 'kv_packed -> 'inner
         and type (_, _, 'entry) external_entry := 'entry
         and type ('inner, _, _, 'entry) with_external_entry := 'entry -> 'inner
         and type ('inner, _) with_decoder := 'inner

    val map : ('k, 'v, 'kv_p, 'e) t -> f:('e -> 'e) -> ('k, 'v, 'kv_p, 'e) t
    val map_inplace : ('k, 'v, 'kv_p, 'e) t -> f:('e -> 'e) -> unit

    val map_poly :
         ('k, 'v1, 'kv_p1, 'e1) t
      -> vtable:('k, 'v2, 'kv_p2, unit, 'e2) vtable
      -> f:('e1 -> 'e2)
      -> ('k, 'v2, 'kv_p2, 'e2) t
  end

  (** {2 Construction} *)

  module Entry_size : sig
    type 'a immediate
    type 'a value1
    type ('a, 'b) value2
    type ('a, 'b, 'c) value3

    type (_, _) t =
      | Immediate : ('a, 'a immediate) t
      | Value1 : ('a, 'a value1) t
      | Value2 : ('a * 'b, ('a, 'b) value2) t
      | Value3 : ('a * 'b * 'c, ('a, 'b, 'c) value3) t
  end

  val create :
       vtable:('key, 'value, 'kv_packed, 'decoder, 'kv_pair) vtable
    -> initial_capacity:int
    -> entry_size:('kv_packed, _) Entry_size.t
    -> unit
    -> ('key, 'value, 'kv_packed, 'decoder, 'kv_pair) t
end

(*————————————————————————————————————————————————————————————————————————————
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
