(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

(* TODO: change the implementation of this module to use [Hashed_container *)

module Bucket = struct
  include Immediate_array

  let cons x xs =
    let old_len = length xs in
    let len = old_len + 1 in
    let dst = create ~len x in
    let dst = unsafe_blit ~src:xs ~dst ~src_pos:0 ~dst_pos:1 ~len:old_len in
    dst
end

(* String elements are stored in an arena (to avoid header words + padding),
   and we keep a hash-set of pointers into the arena. *)
type t = { arena : Arena.t; mutable hashset : Arena.id Bucket.t array }

let hash_elt : string -> int = Hashtbl.hash

let arena_capacity ~bucket_count =
  (* Expand the hashset when there are ~2 elements per bucket *)
  2 * bucket_count

let create ~elt_length ~initial_capacity =
  let bucket_count = max 1 (initial_capacity / 2) in
  let hashset = Array.make bucket_count Bucket.empty in
  let arena =
    Arena.create ~elt_length ~initial_capacity:(arena_capacity ~bucket_count)
  in
  { hashset; arena }

let elt_index t elt = hash_elt elt mod Array.length t.hashset

let mem t elt =
  let bucket = t.hashset.(elt_index t elt) in
  Bucket.exists ~f:(fun id -> Arena.elt_equal t.arena id elt) bucket

let iter_hashset hashset f = Array.iter ~f:(Bucket.iter ~f) hashset

let resize t =
  (* Scale the number of hashset buckets. *)
  let new_bucket_count = (2 * Array.length t.hashset) + 1 in
  let new_hashset = Array.make new_bucket_count Bucket.empty in
  iter_hashset t.hashset (fun index ->
      let elt = Arena.dereference t.arena index in
      let new_index = hash_elt elt mod new_bucket_count in
      new_hashset.(new_index) <- Bucket.cons index new_hashset.(new_index));
  t.hashset <- new_hashset;
  (* Scale the arena size. *)
  Arena.expand t.arena (arena_capacity ~bucket_count:new_bucket_count)

let add t elt =
  if Arena.is_full t.arena then resize t;
  let arena_idx = Arena.allocate t.arena elt in
  (* Add the arena offset to the hashset. *)
  let elt_idx = elt_index t elt in
  t.hashset.(elt_idx) <- Bucket.cons arena_idx t.hashset.(elt_idx)

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
