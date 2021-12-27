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

type ('k, 'v, 'packed, 's, 'entry, 'bucket) unboxed =
  { entry_size : ('packed, 'bucket) Entry_size.t
  ; mutable hashtbl : 'bucket Array.t
  ; mutable bucket_count_log2 : int
  ; mutable cardinal : int
  ; mutable mutation_allowed : bool (* Set during all iteration operations *)
  ; vtable : ('k, 'v, 'packed, 's, 'entry) vtable
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

let create ~vtable ~initial_capacity ~entry_size () =
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
    ; vtable
    }

module T = struct
  let copy (T t) = T { t with hashtbl = Array.copy t.hashtbl }
  let cardinal (T t) = t.cardinal
  let vtable (T t) = t.vtable

  let clear (T t) =
    ensure_mutation_allowed ~__FUNCTION__ t;
    let empty = Bucket.empty t.entry_size in
    t.hashtbl <- [| empty |];
    t.bucket_count_log2 <- 0;
    t.cardinal <- 0

  let elt_index : type a b c d e. (a, b, c, d, e) t -> a -> int =
   fun (T t) key ->
    let v = t.vtable in
    (* NOTE: we use the _uppermost_ bits of the key hash to index the bucket
         array, so that the hashtbl is approximately sorted by key hash (with only
         the entries within each bucket being relatively out of order). *)
    let unneeded_bits = v.key_hash_size - t.bucket_count_log2 in
    (v.key_hash key lsr unneeded_bits) land ((1 lsl t.bucket_count_log2) - 1)

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
      let v = t.vtable in
      let key = v.packed_key decoder packed in
      let new_index = elt_index (T t) key in
      assert (new_index lsr 1 = index);
      new_index land 1 = 0
    in
    fun ~decoder ({ entry_size; _ } as t) index bucket ->
      Bucket.partition3 entry_size bucket ~a:t ~b:decoder ~c:index ~f

  let resize : type a b c d e. (a, b, c, d, e) t -> d -> unit =
   fun (T t) decoder ->
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

  let maybe_resize t ~decoder =
    let should_grow = t.cardinal > 2 * Array.length t.hashtbl in
    if should_grow then resize (T t) decoder

  type add_result = Add | Replace

  let add_worker :
      type a b c d e.
      (a, b, c, d, e) t -> replace:bool -> decoder:d -> a -> c -> add_result =
   fun (T t) ~replace ~decoder key packed ->
    let v = t.vtable in
    if t.cardinal > 2 * Array.length t.hashtbl then resize (T t) decoder;
    let elt_idx = elt_index (T t) key in
    let bucket = t.hashtbl.(elt_idx) in
    let length = Bucket.length t.entry_size bucket in
    let bucket' =
      Bucket.replace t.entry_size bucket ~replace ~decoder ~unpack:v.packed_key
        ~key ~key_equal:v.key_equal ~data:packed
    in
    let length' = Bucket.length t.entry_size bucket' in
    (* Avoid [caml_modify] when new bucket is identical: *)
    if not (bucket == bucket') then t.hashtbl.(elt_idx) <- bucket';
    if length' > length then (
      t.cardinal <- t.cardinal + 1;
      maybe_resize t ~decoder;
      Add)
    else Replace

  let replace (T t) ~decoder key packed =
    ensure_mutation_allowed ~__FUNCTION__ t;
    let (_ : add_result) = add_worker (T t) ~replace:true ~decoder key packed in
    ()

  let add (T t) ~decoder key packed =
    ensure_mutation_allowed ~__FUNCTION__ t;
    match add_worker (T t) ~replace:false ~decoder key packed with
    | Add -> `Ok
    | Replace -> `Duplicate

  let add_exn t ~decoder key packed =
    match add t ~decoder key packed with
    | `Ok -> ()
    | `Duplicate ->
        Printf.ksprintf failwith "%s: got key already present" __FUNCTION__

  let remove : type a b c d e. (a, b, c, d, e) t -> decoder:d -> a -> unit =
   fun (T t) ~decoder key ->
    ensure_mutation_allowed ~__FUNCTION__ t;
    let v = t.vtable in
    let elt_idx = elt_index (T t) key in
    let bucket = t.hashtbl.(elt_idx) in
    let key_found = ref false in
    let bucket' =
      Bucket.fold_left t.entry_size bucket ~init:[] ~f:(fun acc entry ->
          if !key_found then
            (* We ensure there's at most one binding for a given key *)
            entry :: acc
          else
            let key' = v.packed_key decoder entry in
            match v.key_equal key key' with
            | false -> entry :: acc
            | true ->
                (* Drop this binding *)
                key_found := true;
                acc)
      |> Bucket.of_list_rev t.entry_size
    in
    match !key_found with
    | false -> (* Nothing to do *) ()
    | true ->
        t.cardinal <- t.cardinal - 1;
        t.hashtbl.(elt_idx) <- bucket'

  let mem : type a b c d e. (a, b, c, d, e) t -> decoder:d -> a -> bool =
   fun (T t) ~decoder key ->
    let v = t.vtable in
    let elt_idx = elt_index (T t) key in
    let bucket = t.hashtbl.(elt_idx) in
    Bucket.exists t.entry_size bucket ~f:(fun packed ->
        v.key_equal key (v.packed_key decoder packed))

  let find_and_call :
      type k v c d e r.
         (k, v, c, d, e) t
      -> decoder:d
      -> k
      -> if_found:(key:k -> data:v -> r)
      -> if_not_found:(k -> r)
      -> r =
   fun (T t) ~decoder key ~if_found ~if_not_found ->
    let v = t.vtable in
    let elt_idx = elt_index (T t) key in
    let bucket = t.hashtbl.(elt_idx) in
    match
      Bucket.find_map t.entry_size bucket ~f:(fun packed ->
          (* We expect the keys to match most of the time, so we decode the
             value at the same time. *)
          let entry = v.packed_entry decoder packed in
          match v.key_equal key (v.entry_key entry) with
          | false -> None
          | true -> Some entry)
    with
    (* TODO: avoid option allocation here *)
    | Some entry ->
        if_found ~key:(v.entry_key entry) ~data:(v.entry_value entry)
    | None -> if_not_found key

  let find =
    let if_found ~key:_ ~data = Some data in
    let if_not_found _ = None in
    fun t ~decoder key -> find_and_call t key ~decoder ~if_found ~if_not_found

  let find_exn =
    let if_found ~key:_ ~data = data in
    let if_not_found _ = raise Not_found in
    fun t ~decoder key -> find_and_call t key ~decoder ~if_found ~if_not_found

  let fold :
      type a b c d e acc.
      (a, b, c, d, e) t -> decoder:d -> f:(acc -> e -> acc) -> init:acc -> acc =
   fun (T t) ~decoder ~f ~init ->
    let v = t.vtable in
    with_mutation_disallowed t ~f:(fun () ->
        Array.fold_left t.hashtbl ~init ~f:(fun acc bucket ->
            Bucket.fold_left t.entry_size bucket ~init:acc ~f:(fun acc packed ->
                let entry = v.packed_entry decoder packed in
                f acc entry)))

  let map_poly :
      type a v1 v2 c1 c2 d1 d2 e1 e2.
         (a, v1, c1, d1, e1) t
      -> vtable:(a, v2, c2, d2, e2) vtable
      -> decoder_src:d1
      -> decoder_dst:d2
      -> f:(e1 -> e2)
      -> (a, v2, c2, d2, e2) t =
   fun (T t) ~vtable ~decoder_src ~decoder_dst ~f ->
    let v = t.vtable in
    let hashtbl =
      with_mutation_disallowed t ~f:(fun () ->
          Array.map t.hashtbl ~f:(fun bucket ->
              Bucket.map t.entry_size
                (Obj.magic
                   t.entry_size (* TODO: use [Higher] typing for entry_size *))
                bucket ~f:(fun packed ->
                  let entry = v.packed_entry decoder_src packed in
                  let entry' = f entry in
                  vtable.packed_of_entry decoder_dst entry')))
    in
    T
      { t with
        entry_size = Obj.magic t.entry_size
      ; hashtbl
      ; vtable
      ; mutation_allowed = true
      }

  let map (T t) ~decoder_src ~decoder_dst ~f =
    map_poly (T t) ~vtable:t.vtable ~decoder_src ~decoder_dst ~f

  let map_inplace :
      type a b c d e.
      (a, b, c, d, e) t -> decoder_src:d -> decoder_dst:d -> f:(e -> e) -> unit
      =
   fun (T t) ~decoder_src ~decoder_dst ~f ->
    let v = t.vtable in
    with_mutation_disallowed t ~f:(fun () ->
        Array.map_inplace t.hashtbl ~f:(fun bucket ->
            Bucket.map_inplace t.entry_size bucket ~f:(fun packed ->
                let entry = v.packed_entry decoder_src packed in
                let entry' = f entry in
                v.packed_of_entry decoder_dst entry')))

  let iter :
      type a b c d e. (a, b, c, d, e) t -> decoder:d -> f:(e -> unit) -> unit =
   fun (T t) ~decoder ~f ->
    let v = t.vtable in
    with_mutation_disallowed t ~f:(fun () ->
        Array.iter t.hashtbl ~f:(fun bucket ->
            Bucket.iter t.entry_size bucket ~f:(fun packed ->
                f (v.packed_entry decoder packed))))

  let iter_keys :
      type a b c d e. (a, b, c, d, e) t -> decoder:d -> f:(a -> unit) -> unit =
   fun (T t) ~decoder ~f ->
    let v = t.vtable in
    with_mutation_disallowed t ~f:(fun () ->
        Array.iter t.hashtbl ~f:(fun bucket ->
            Bucket.iter t.entry_size bucket ~f:(fun packed ->
                f (v.packed_key decoder packed))))

  let exists t ~decoder ~f =
    let exception Exit in
    match iter t ~decoder ~f:(fun e -> if f e then raise Exit) with
    | exception Exit -> true
    | () -> false

  let for_all t ~decoder ~f = not (exists t ~decoder ~f:(fun e -> not (f e)))

  let count t ~decoder ~f =
    fold t ~decoder ~init:0 ~f:(fun acc e -> if f e then acc + 1 else acc)

  let to_list t ~decoder =
    let acc = fold t ~decoder ~init:[] ~f:(fun acc e -> e :: acc) in
    List.rev acc

  let to_sorted_seq : type a b c d e. (a, b, c, d, e) t -> decoder:d -> e Seq.t
      =
   fun (T t) ~decoder ->
    let v = t.vtable in
    Array.to_seq t.hashtbl
    |> Seq.flat_map (fun bucket ->
           let arr =
             Bucket.to_array t.entry_size bucket
             |> Array.map ~f:(fun off -> v.packed_entry decoder off)
           in
           Array.sort ~cmp:v.entry_compare arr;
           Array.to_seq arr)

  let reserve _ = failwith "TODO"
  let bucket_count (T t) = Array.length t.hashtbl

  let load_factor (T t) =
    Float.of_int t.cardinal /. Float.of_int (Array.length t.hashtbl)

  let invariant :
      type a b c d e.
      decoder:d -> a Invariant.t -> b Invariant.t -> (a, b, c, d, e) t -> unit =
   fun ~decoder invariant_key invariant_data (T t) ->
    let v = t.vtable in
    for i = 0 to Array.length t.hashtbl - 1 do
      Bucket.invariant t.entry_size
        (fun packed ->
          let entry = v.packed_entry decoder packed in
          invariant_key (v.entry_key entry);
          invariant_data (v.entry_value entry))
        t.hashtbl.(i)
    done;
    let real_length = fold ~decoder (T t) ~init:0 ~f:(fun i _ -> i + 1) in
    assert (t.cardinal = real_length)
end

include T

module No_decoder = struct
  include T

  type nonrec ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, unit, 'd) t

  let decoder = ()
  let find = find ~decoder
  let find_and_call = find_and_call ~decoder
  let find_exn = find_exn ~decoder
  let remove = remove ~decoder
  let mem = mem ~decoder
  let iter = iter ~decoder
  let iter_keys = iter_keys ~decoder
  let map = map ~decoder_src:decoder ~decoder_dst:decoder
  let map_poly = map_poly ~decoder_src:decoder ~decoder_dst:decoder
  let map_inplace = map_inplace ~decoder_src:decoder ~decoder_dst:decoder
  let fold = fold ~decoder
  let count = count ~decoder
  let exists = exists ~decoder
  let for_all = for_all ~decoder
  let to_list = to_list ~decoder
  let to_sorted_seq = to_sorted_seq ~decoder
  let add = add ~decoder
  let add_exn = add_exn ~decoder
  let replace = replace ~decoder
  let invariant k v t = invariant ~decoder k v t
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
