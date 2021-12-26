(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(** A polymorphic hashset of elements of arbitrary type. For more
    memory-efficient implementations for specific element types, see the
    provided {{!type-specialisations} type specialisations}. *)

type 'a t

module type Key = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val hash_size : int
end

val create : initial_capacity:int -> (module Key with type t = 'a) -> 'a t

(** @inline *)
include Hashed_container.Set with type 'a t := 'a t and type 'a key := 'a

module Internal :
  Internal.S1
    with type 'a outer_t := 'a t
     and type 'a t = ('a, unit, 'a, unit, 'a) Hashed_container.t

(** {1:type-specialisations Type specialisations} *)

module Immediate : sig
  (** An hashset of elements with an {i immediate} runtime representation (such
      that [Obj.is_int] always holds). Attempting to store non-immediate values
      in this hashset will raise an exception.

      See {{!implementation} below} for an explanation of the implementation. *)

  include Hashed_container.Set with type 'a key := 'a
  (** @inline *)

  val create : initial_capacity:int -> (module Key with type t = 'a) -> 'a t

  (** {1:implementation Implementation details}

      Restricting the elements to always be immediates allows a more efficient
      implementation of the hashset in which buckets of size 1 can be stored
      directly in the parent array (rather than allocating a separate heap block
      for the singleton bucket, as done by the standard implementation). Buckets
      with more than a single element still use separate chaining, with an
      overhead of two words.

      For example, consider the following hashset of the characters
      ['a' ... 'd']:

      {v
          ┌─────┐
          │ hdr │
          ├─────┤
          │ { } │
          ├─────┤
          │ 'a' │
          ├─────┤    ┌─────┬─────┬─────┐
          │  ┄┄┄┼┄┄┄>│ hdr │ 'c' │ 'b' │
          ├─────┤    └─────┴─────┴─────┘
          │ 'd' │
          ├─────┤
          │ { } │
          └─────┘
      v}

      For typical load factors, inlining singleton buckets into the parent array
      is a considerable memory reduction (~20%), and avoids some unnecessary
      allocations. *)
end

(** [Immediate64] is like [Immediate] but for types that are only guaranteed to
    have an immediate representation when [Sys.word_size = 64], such as
    [Int63.t]. *)
module Immediate64 : sig
  include Hashed_container.Set with type 'a key := 'a
  (** @inline *)

  val create : initial_capacity:int -> (module Key with type t = 'a) -> 'a t
end

module Fixed_size_string = Hashset_fixed_size_string

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
