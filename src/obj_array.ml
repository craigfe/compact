(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2016–2020 Jane Street Group, LLC <opensource@janestreet.com>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(** Code in this module has been extracted from Jane Street's [base] library,
    with minimal modifications. *)

open! Import

(* We maintain the property that all values of type [t] do not have the tag
   [double_array_tag]. Some functions below assume this in order to avoid
   testing the tag, and will segfault if this property doesn't hold. *)
type t = Obj.t array

let invariant t = assert (Obj.tag (Obj.repr t) <> Obj.double_array_tag)
let length = Array.length

let swap t i j =
  let tmp = Array.get t i in
  Array.set t i (Array.get t j);
  Array.set t j tmp

let zero_obj = Obj.repr (0 : int)

(* We call [Array.create] with a value that is not a float so that the array doesn't get
   tagged with [Double_array_tag]. *)
let create_zero ~len = Array.make len zero_obj

let create ~len x =
  (* If we can, use [Array.create] directly. *)
  if Obj.tag x <> Obj.double_tag then Array.make len x
  else
    (* Otherwise use [create_zero] and set the contents *)
    let t = create_zero ~len in
    let x = Sys.opaque_identity x in
    for i = 0 to len - 1 do
      Array.unsafe_set t i x
    done;
    t

let empty = [||]

type not_a_float = Not_a_float_0 | Not_a_float_1 of int

let _not_a_float_0 = Not_a_float_0
let _not_a_float_1 = Not_a_float_1 42

let get t i =
  (* Make the compiler believe [t] is an array not containing floats so it does not check
     if [t] is tagged with [Double_array_tag].  It is NOT ok to use [int array] since (if
     this function is inlined and the array contains in-heap boxed values) wrong register
     typing may result, leading to a failure to register necessary GC roots. *)
  Obj.repr ((Obj.magic (t : t) : not_a_float array).(i) : not_a_float)

let[@inline always] unsafe_get t i =
  (* Make the compiler believe [t] is an array not containing floats so it does not check
     if [t] is tagged with [Double_array_tag]. *)
  Obj.repr
    (Array.unsafe_get (Obj.magic (t : t) : not_a_float array) i : not_a_float)

let[@inline always] unsafe_set_with_caml_modify t i obj =
  (* Same comment as [unsafe_get]. Sys.opaque_identity prevents the compiler from
     potentially wrongly guessing the type of the array based on the type of element, that
     is prevent the implication: (Obj.tag obj = Obj.double_tag) => (Obj.tag t =
     Obj.double_array_tag) which flambda has tried in the past (at least that's assuming
     the compiler respects Sys.opaque_identity, which is not always the case). *)
  Array.unsafe_set
    (Obj.magic (t : t) : not_a_float array)
    i
    (Obj.obj (Sys.opaque_identity obj) : not_a_float)

let[@inline always] unsafe_set_int_assuming_currently_int t i int =
  (* This skips [caml_modify], which is OK if both the old and new values are integers. *)
  Array.unsafe_set (Obj.magic (t : t) : int array) i (Sys.opaque_identity int)

(* For [set] and [unsafe_set], if a pointer is involved, we first do a physical-equality
   test to see if the pointer is changing.  If not, we don't need to do the [set], which
   saves a call to [caml_modify].  We think this physical-equality test is worth it
   because it is very cheap (both values are already available from the [is_int] test)
   and because [caml_modify] is expensive. *)

let set t i obj =
  (* We use [get] first but then we use [Array.unsafe_set] since we know that [i] is
     valid. *)
  let old_obj = get t i in
  if Obj.is_int old_obj && Obj.is_int obj then
    unsafe_set_int_assuming_currently_int t i (Obj.obj obj : int)
  else if not (old_obj == obj) then unsafe_set_with_caml_modify t i obj

let[@inline always] unsafe_set t i obj =
  let old_obj = unsafe_get t i in
  if Obj.is_int old_obj && Obj.is_int obj then
    unsafe_set_int_assuming_currently_int t i (Obj.obj obj : int)
  else if not (old_obj == obj) then unsafe_set_with_caml_modify t i obj

let[@inline always] unsafe_set_omit_phys_equal_check t i obj =
  let old_obj = unsafe_get t i in
  if Obj.is_int old_obj && Obj.is_int obj then
    unsafe_set_int_assuming_currently_int t i (Obj.obj obj : int)
  else unsafe_set_with_caml_modify t i obj

let singleton obj = create ~len:1 obj

(* Pre-condition: t.(i) is an integer. *)
let unsafe_set_assuming_currently_int t i obj =
  if Obj.is_int obj then
    unsafe_set_int_assuming_currently_int t i (Obj.obj obj : int)
  else
    (* [t.(i)] is an integer and [obj] is not, so we do not need to check if they are
       equal. *)
    unsafe_set_with_caml_modify t i obj

let unsafe_set_int t i int =
  let old_obj = unsafe_get t i in
  if Obj.is_int old_obj then unsafe_set_int_assuming_currently_int t i int
  else unsafe_set_with_caml_modify t i (Obj.repr int)

let unsafe_clear_if_pointer t i =
  let old_obj = unsafe_get t i in
  if not (Obj.is_int old_obj) then unsafe_set_with_caml_modify t i (Obj.repr 0)

(** [unsafe_blit] is like [Array.blit], except it uses our own for-loop to avoid
    caml_modify when possible. Its performance is still not comparable to a
    memcpy. *)
let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
  (* When [phys_equal src dst], we need to check whether [dst_pos < src_pos] and have the
     for loop go in the right direction so that we don't overwrite data that we still need
     to read.  When [not (phys_equal src dst)], doing this is harmless.  From a
     memory-performance perspective, it doesn't matter whether one loops up or down.
     Constant-stride access, forward or backward, should be indistinguishable (at least on
     an intel i7).  So, we don't do a check for [phys_equal src dst] and always loop up in
     that case. *)
  if dst_pos < src_pos then
    for i = 0 to len - 1 do
      unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
    done
  else
    for i = len - 1 downto 0 do
      unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
    done

(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2016–2020 Jane Street Group, LLC <opensource@janestreet.com>

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
