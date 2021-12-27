(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(** Extra functions are exposed here when needed by the [Hashed_container]
    implementation, and so the API is quite arbitrary by itself. *)

module Entry_size : sig
  type 'a immediate = 'a Immediate_array.t
  type 'a value1 = 'a Uniform_array.t
  type ('a, 'b) value2 = ('a, 'b) Uniform_array.Tuple2.t
  type ('a, 'b, 'c) value3 = ('a, 'b, 'c) Uniform_array.Tuple3.t

  type (_, _) t =
    | Immediate : ('a, 'a immediate) t
    | Value1 : ('a, 'a value1) t
    | Value2 : ('a * 'b, ('a, 'b) value2) t
    | Value3 : ('a * 'b * 'c, ('a, 'b, 'c) value3) t
end

val empty : ('a, 'b) Entry_size.t -> 'b
val length : ('a, 'b) Entry_size.t -> 'b -> int
val cons : ('a, 'b) Entry_size.t -> 'a -> 'b -> 'b
val exists : ('a, 'b) Entry_size.t -> f:('a -> bool) -> 'b -> bool

val fold_left :
  ('a, 'b) Entry_size.t -> f:('acc -> 'a -> 'acc) -> init:'acc -> 'b -> 'acc

val map :
     ('a1, 'b1) Entry_size.t
  -> ('a2, 'b2) Entry_size.t
  -> f:('a1 -> 'a2)
  -> 'b1
  -> 'b2

val map_inplace : ('a, 'b) Entry_size.t -> f:('a -> 'a) -> 'b -> 'b
val iter : ('a, 'b) Entry_size.t -> f:('a -> unit) -> 'b -> unit
val to_array : ('a, 'b) Entry_size.t -> 'b -> 'a array
val of_list_rev : ('a, 'b) Entry_size.t -> 'a list -> 'b
val find_map : ('a, 'b) Entry_size.t -> f:('a -> 'r option) -> 'b -> 'r option

val partition3 :
     ('a, 'b) Entry_size.t
  -> 'b
  -> a:'s1
  -> b:'s2
  -> c:'s3
  -> f:(a:'s1 -> b:'s2 -> c:'s3 -> 'a -> bool)
  -> 'b * 'b

val replace :
     ('a, 'b) Entry_size.t
  -> 'b
  -> decoder:'d
  -> unpack:('d -> 'a -> 'k)
  -> key:'k
  -> key_equal:('k -> 'k -> bool)
  -> replace:bool
  -> data:'a
  -> 'b

val invariant : ('a, 'b) Entry_size.t -> 'a Invariant.t -> 'b Invariant.t

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
