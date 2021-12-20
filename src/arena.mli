(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

type t

val create : elt_length:int -> initial_capacity:int -> t
(** [create ~elt_length:len ~initial_capacity:n] is an empty arena of strings of
    length [len], with space sufficient to store [n] values. *)

val is_full : t -> bool
(** [is_full t] is true iff [t] has no remaining space for elements. *)

val expand : t -> int -> unit
(** [expand t n] re-allocates arena [t] to support storing up to [n]-many
    elements. Existing elements in the arena retain their original {!id}s.

    @raise Invalid_argument if [n] is less than the current capacity of [t]. *)

type id
(** The type of references to allocated elements in an arena. *)

val allocate : t -> string -> id
(** [allocate t s] adds the string [s] to arena [t], returning a reference to
    the storage location that may later be {!dereference}d to get back [s].

    @raise Invalid_argument
      if [t] {!is_full}. The behaviour is undefined if the length of [s] is not
      equal to the [elt_length] of [t]. *)

val dereference : t -> id -> string
(** [dereference t id] is the string that was passed to the {!allocate} call
    that returned [id]. The behaviour is undefined if [id] was not created by an
    allocation within [t]. *)

val elt_equal : t -> id -> string -> bool
(** [elt_equal t id s] is equivalent to [String.equal (dereference t id) s], but
    more efficient. *)

type internal =
  { elt_length : int; mutable data : Bigstringaf.t; mutable next_offset : int }

module Internal : Internal.S0 with type outer_t := t and type t = internal

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
