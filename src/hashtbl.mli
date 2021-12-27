(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

type ('k, 'v) t

(** @inline *)
include
  Hashed_container.Assoc
    with type ('k, 'v, _, _, _) t := ('k, 'v) t
     and type 'k key := 'k
     and type ('k, 'v, _) external_entry := 'k * 'v
     and type ('inner, 'k, 'v, _) with_external_entry :=
      key:'k -> data:'v -> 'inner
     and type ('inner, 'k, 'v, _) with_internal_entry :=
      key:'k -> data:'v -> 'inner
     and type ('r, _) with_decoder := 'r

val map : ('k, 'v1) t -> f:('v1 -> 'v2) -> ('k, 'v2) t
val map_inplace : ('k, 'v) t -> f:('v -> 'v) -> unit

module type Key = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val hash_size : int
end

val create : initial_capacity:int -> (module Key with type t = 'k) -> ('k, _) t
val create_poly : initial_capacity:int -> unit -> (_, _) t

module Internal :
  Internal.S2
    with type ('a, 'b) outer_t := ('a, 'b) t
     and type ('a, 'b) t = ('a, 'b, 'a * 'b, unit, 'a * 'b) Hashed_container.t

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
