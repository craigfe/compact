(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(* This module defines the hashtable implementation that underlies most
   containers in this library. *)

open! Import
include Hashed_container_intf
include Hashed_container_intf.Types
module Bucket = Hashed_container_bucket
module Entry_size = Bucket.Entry_size

type ('k, 'v, 'packed, 's, 'entry, 'bucket) unboxed =
  { entry_size : ('packed, 'bucket) Entry_size.t
  ; mutable hashtbl : 'bucket Array.t
  ; mutable bucket_count_log2 : int
  ; mutable cardinal : int
  ; mutable mutation_allowed : bool (* Set during all iteration operations *)
  ; key_impl : ('k, 's, 'packed) key_impl
  ; entry_impl : ('k, 'v, 'packed, 's, 'entry) entry_impl
  }

type (_, _, _, _, _) internal =
  | T :
      ('k, 'v, 'packed, 's, 'entry, _) unboxed
      -> ('k, 'v, 'packed, 's, 'entry) internal
[@@ocaml.unboxed]

type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) internal

let ensure_mutation_allowed ~__FUNCTION__:ctx t =
  if not t.mutation_allowed then
    Format.kasprintf failwith "%s: mutation not allowed during iteration" ctx

let with_mutation_disallowed t ~f =
  let m = t.mutation_allowed in
  t.mutation_allowed <- false;
  match f () with
  | a ->
      t.mutation_allowed <- m;
      a
  | exception exn ->
      t.mutation_allowed <- m;
      raise exn

let create ~key:key_impl ~entry:entry_impl ~initial_capacity ~entry_size () =
  let bucket_count_log2, bucket_count =
    let rec aux n_log2 n =
      if n >= initial_capacity then (n_log2, n)
      else if n * 2 > Sys.max_array_length then (n_log2, n)
      else aux (n_log2 + 1) (n * 2)
    in
    aux 4 16
  in
  let hashtbl = Array.make bucket_count (Bucket.empty entry_size) in
  T
    { hashtbl
    ; bucket_count_log2
    ; cardinal = 0
    ; mutation_allowed = true
    ; entry_size
    ; key_impl
    ; entry_impl
    }

module T = struct
  let copy (T t) = T { t with hashtbl = Array.copy t.hashtbl }
  let cardinal (T t) = t.cardinal

  let clear (T t) =
    ensure_mutation_allowed ~__FUNCTION__ t;
    let empty = Bucket.empty t.entry_size in
    t.hashtbl <- [| empty |];
    t.bucket_count_log2 <- 0;
    t.cardinal <- 0

  let elt_index : type a b c d e. (a, b, c, d, e) t -> a -> int =
   fun (T t) key ->
    let (module Key) = t.key_impl in
    (* NOTE: we use the _uppermost_ bits of the key hash to index the bucket
         array, so that the hashtbl is approximately sorted by key hash (with only
         the entries within each bucket being relatively out of order). *)
    let unneeded_bits = Key.hash_size - t.bucket_count_log2 in
    (Key.hash key lsr unneeded_bits) land ((1 lsl t.bucket_count_log2) - 1)

  let partition_bucket :
      type a b c d e bucket.
         decoder:d
      -> (a, b, c, d, e, bucket) unboxed
      -> int
      -> bucket
      -> bucket * bucket =
    let f :
        type a b c d e.
        a:(a, b, c, d, e, _) unboxed -> b:d -> c:int -> c -> bool =
     fun ~a:t ~b:decoder ~c:index packed ->
      let (module Key) = t.key_impl in
      let key = Key.unpack decoder packed in
      let new_index = elt_index (T t) key in
      assert (new_index lsr 1 = index);
      new_index land 1 = 0
    in
    fun ~decoder ({ entry_size; _ } as t) index bucket ->
      Bucket.partition3 entry_size bucket ~a:t ~b:decoder ~c:index ~f

  let resize : type a b c d e. (a, b, c, d, e) t -> d -> unit =
   fun (T t) decoder ->
    let (module Key) = t.key_impl in
    (* Scale the number of hashtbl buckets. *)
    t.bucket_count_log2 <- t.bucket_count_log2 + 1;
    let new_bucket_count = 1 lsl t.bucket_count_log2 in
    if new_bucket_count > Sys.max_array_length then
      Format.kasprintf failwith
        "Log_file.resize: can't construct a hashtbl with %d buckets \
         (Sys.max_array_length = %d)"
        new_bucket_count Sys.max_array_length;
    let new_hashtbl = Array.make new_bucket_count (Bucket.empty t.entry_size) in
    Array.iteri t.hashtbl ~f:(fun i bucket ->
        (* The bindings in this bucket will be split into two new buckets, using
           the next bit of [Key.hash] as a discriminator. *)
        let bucket_2i, bucket_2i_plus_1 =
          partition_bucket ~decoder t i bucket
        in
        new_hashtbl.(2 * i) <- bucket_2i;
        new_hashtbl.((2 * i) + 1) <- bucket_2i_plus_1);
    t.hashtbl <- new_hashtbl

  (** Replace implementation that only updates in-memory state (and doesn't
      write the binding to disk). *)
  let replace : type a b c d e. (a, b, c, d, e) t -> decoder:d -> a -> c -> unit
      =
   fun (T t) ~decoder key offset ->
    ensure_mutation_allowed ~__FUNCTION__ t;
    let (module Key) = t.key_impl in
    if t.cardinal > 2 * Array.length t.hashtbl then resize (T t) decoder;
    let elt_idx = elt_index (T t) key in
    let bucket = t.hashtbl.(elt_idx) in
    let length = Bucket.length t.entry_size bucket in
    let bucket =
      Bucket.replace t.entry_size bucket ~decoder ~unpack:Key.unpack ~key
        ~key_equal:Key.equal ~data:offset
    in
    let length' = Bucket.length t.entry_size bucket in
    if length' > length then t.cardinal <- t.cardinal + 1;
    t.hashtbl.(elt_idx) <- bucket

  let remove : type a b c d e. (a, b, c, d, e) t -> decoder:d -> a -> unit =
   fun (T t) ~decoder key ->
    ensure_mutation_allowed ~__FUNCTION__ t;
    let (module Key) = t.key_impl in
    let elt_idx = elt_index (T t) key in
    let bucket = t.hashtbl.(elt_idx) in
    let key_found = ref false in
    let bucket' =
      Bucket.fold_left t.entry_size bucket ~init:[] ~f:(fun acc offset' ->
          if !key_found then
            (* We ensure there's at most one binding for a given key *)
            acc
          else
            let key' = Key.unpack decoder offset' in
            match Key.equal key key' with
            | false -> offset' :: acc
            | true -> (* Drop this binding *) acc)
      |> Bucket.of_list_rev t.entry_size
    in
    match !key_found with
    | false -> (* Nothing to do *) ()
    | true -> t.hashtbl.(elt_idx) <- bucket'

  let mem : type a b c d e. (a, b, c, d, e) t -> decoder:d -> a -> bool =
   fun (T t) ~decoder key ->
    let (module Key) = t.key_impl and (module Entry) = t.entry_impl in
    let elt_idx = elt_index (T t) key in
    let bucket = t.hashtbl.(elt_idx) in
    Bucket.exists t.entry_size bucket ~f:(fun offset ->
        Key.equal key (Key.unpack decoder offset))

  let find : type a b c d e. (a, b, c, d, e) t -> decoder:d -> a -> b option =
   fun (T t) ~decoder key ->
    let (module Key) = t.key_impl and (module Entry) = t.entry_impl in
    let elt_idx = elt_index (T t) key in
    let bucket = t.hashtbl.(elt_idx) in
    Bucket.find_map t.entry_size bucket ~f:(fun offset ->
        (* We expect the keys to match most of the time, so we decode the
           value at the same time. *)
        let entry = Entry.unpack decoder offset in
        match Key.equal key (Entry.key entry) with
        | false -> None
        | true -> Some (Entry.value entry))

  let find_exn t ~decoder key =
    match find t ~decoder key with None -> raise Not_found | Some x -> x

  let fold :
      type a b c d e acc.
      (a, b, c, d, e) t -> decoder:d -> f:(acc -> e -> acc) -> init:acc -> acc =
   fun (T t) ~decoder ~f ~init ->
    let (module Entry) = t.entry_impl in
    with_mutation_disallowed t ~f:(fun () ->
        Array.fold_left t.hashtbl ~init ~f:(fun acc bucket ->
            Bucket.fold_left t.entry_size bucket ~init:acc ~f:(fun acc offset ->
                let entry = Entry.unpack decoder offset in
                f acc entry)))

  let iter :
      type a b c d e. (a, b, c, d, e) t -> decoder:d -> f:(e -> unit) -> unit =
   fun (T t) ~decoder ~f ->
    let (module Entry) = t.entry_impl in
    with_mutation_disallowed t ~f:(fun () ->
        Array.iter t.hashtbl ~f:(fun bucket ->
            Bucket.iter t.entry_size bucket ~f:(fun offset ->
                f (Entry.unpack decoder offset))))

  let count t ~decoder ~f =
    fold t ~decoder ~init:0 ~f:(fun acc e -> if f e then acc + 1 else acc)

  let to_sorted_seq : type a b c d e. (a, b, c, d, e) t -> decoder:d -> e Seq.t
      =
   fun (T t) ~decoder ->
    let (module Entry) = t.entry_impl in
    Array.to_seq t.hashtbl
    |> Seq.flat_map (fun bucket ->
           let arr =
             Bucket.to_array t.entry_size bucket
             |> Array.map ~f:(fun off -> Entry.unpack decoder off)
           in
           Array.sort ~cmp:Entry.compare arr;
           Array.to_seq arr)

  let reserve _ = failwith "TODO"
  let bucket_count (T t) = Array.length t.hashtbl

  let load_factor (T t) =
    Float.of_int t.cardinal /. Float.of_int (Array.length t.hashtbl)
end

include T

module No_decoder = struct
  include T

  type nonrec ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, unit, 'd) t

  let decoder = ()
  let find = find ~decoder
  let find_exn = find_exn ~decoder
  let remove = remove ~decoder
  let mem = mem ~decoder
  let iter = iter ~decoder
  let fold = fold ~decoder
  let count = count ~decoder
  let to_sorted_seq = to_sorted_seq ~decoder
  let replace = replace ~decoder
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
