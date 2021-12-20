(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

(** The representation of buckets uses small lists specialised to a particular
    entry size. *)

module I1 = Immediate_array
module L1 = Uniform_array
module L2 = Uniform_array.Tuple2
module L3 = Uniform_array.Tuple3

module Entry_size = struct
  type 'a immediate = 'a I1.t
  type 'a value1 = 'a L1.t
  type ('a, 'b) value2 = ('a, 'b) L2.t
  type ('a, 'b, 'c) value3 = ('a, 'b, 'c) L3.t

  type (_, _) t =
    | Immediate : ('a, 'a immediate) t
    | Value1 : ('a, 'a value1) t
    | Value2 : ('a * 'b, ('a, 'b) value2) t
    | Value3 : ('a * 'b * 'c, ('a, 'b, 'c) value3) t
end

let empty : type a b. (a, b) Entry_size.t -> b = function
  | Immediate -> I1.empty
  | Value1 -> L1.empty
  | Value2 -> L2.empty
  | Value3 -> L3.empty

let length : type a b. (a, b) Entry_size.t -> b -> int =
 fun entry_size t ->
  match entry_size with
  | Immediate -> I1.length t
  | Value1 -> L1.length t
  | Value2 -> L2.length t
  | Value3 -> L3.length t

let curry2 f a b = f (a, b)
let curry3 f a b c = f (a, b, c)

let list_rev_partition3 =
  let rec part yes no ~f ~a ~b ~c = function
    | [] -> (yes, no)
    | x :: l ->
        if f ~a ~b ~c x then part ~f ~a ~b ~c (x :: yes) no l
        else part ~f ~a ~b ~c yes (x :: no) l
  in
  fun l ~f ~a ~b ~c -> part ~f ~a ~b ~c [] [] l

let partition3 :
    type a b s1 s2 s3.
       (a, b) Entry_size.t
    -> b
    -> a:s1
    -> b:s2
    -> c:s3
    -> f:(a:s1 -> b:s2 -> c:s3 -> a -> bool)
    -> b * b =
 fun entry_size t ~a ~b ~c ~f ->
  match entry_size with
  | Immediate -> (
      match I1.length t with
      | 0 -> (I1.empty, I1.empty)
      | 1 ->
          let elt = I1.unsafe_get t 0 in
          if f ~a ~b ~c elt then (I1.singleton elt, I1.empty)
          else (I1.empty, I1.singleton elt)
      | 2 -> (
          let elt0 = I1.unsafe_get t 0 and elt1 = I1.unsafe_get t 1 in
          match (f ~a ~b ~c elt0, f ~a ~b ~c elt1) with
          | true, false -> (I1.singleton elt0, I1.singleton elt1)
          | false, true -> (I1.singleton elt1, I1.singleton elt0)
          | (true as both_left), true | (false as both_left), false ->
              let both = I1.create ~len:2 elt0 in
              let both = I1.unsafe_set both 1 elt1 in
              if both_left then (both, I1.empty) else (I1.empty, both))
      | n when n < Sys.int_size ->
          (* If the bucket length is less than the number of bits in an integer
             (which is practically always the case for a well-distributed hash
             function), we would use the bits of an integer to record
             partitioning choices and then allocate arrays of precisely the
             correct size. *)
          let bitv = ref 0 in
          let popcount = ref 0 in
          for i = 0 to n - 1 do
            let elt = I1.unsafe_get t i in
            if f ~a ~b ~c elt then (
              bitv := !bitv lor (1 lsl i);
              incr popcount)
          done;
          let bitv = !bitv in
          let left_size = !popcount and right_size = n - !popcount in
          let dummy = I1.unsafe_get t 0 in
          let left = ref (I1.create ~len:left_size dummy)
          and right = ref (I1.create ~len:right_size dummy) in
          let added_to_left = ref 0 in
          for i = 0 to n - 1 do
            match (bitv lsr i) land 1 with
            | 1 ->
                left := I1.unsafe_set !left !added_to_left (I1.unsafe_get t i);
                incr added_to_left
            | 0 ->
                right :=
                  I1.unsafe_set !right (i - !added_to_left) (I1.unsafe_get t i)
            | _ -> assert false
          done;
          (!left, !right)
      | _ ->
          let left, right = I1.to_list t |> list_rev_partition3 ~f ~a ~b ~c in
          (I1.of_list_rev left, I1.of_list_rev right))
  | Value1 ->
      let left, right = L1.to_list t |> list_rev_partition3 ~f ~a ~b ~c in
      (L1.of_list_rev left, L1.of_list_rev right)
  | Value2 ->
      let left, right = L2.to_list t |> list_rev_partition3 ~f ~a ~b ~c in
      (L2.of_list_rev left, L2.of_list_rev right)
  | Value3 ->
      let left, right = L3.to_list t |> list_rev_partition3 ~f ~a ~b ~c in
      (L3.of_list_rev left, L3.of_list_rev right)

exception Exit

let replace :
    type a b d k.
       (a, b) Entry_size.t
    -> b
    -> decoder:d
    -> unpack:(d -> a -> k)
    -> key:k
    -> key_equal:(k -> k -> bool)
    -> data:a
    -> b =
 fun entry_size t ~decoder ~unpack ~key ~key_equal ~data ->
  match entry_size with
  | Immediate -> (
      let length = I1.length t in
      match length with
      | 0 -> I1.singleton data
      | 1 -> (
          (* Handle the length = 1 case separately because it's an immediate. *)
          let x = I1.unsafe_get t 0 in
          let key' = unpack decoder x in
          match key_equal key key' with
          | true -> I1.singleton data
          | false ->
              let res = I1.create ~len:2 x in
              I1.unsafe_set res 0 data)
      | _ -> (
          try
            for i = 0 to length - 1 do
              let x = I1.unsafe_get t i in
              let key' = unpack decoder x in
              match key_equal key key' with
              | true ->
                  let (_ : _ I1.t) = I1.unsafe_set t i data in
                  raise_notrace Exit
              | false -> ()
            done;
            (* Key was not found: add it to the start *)
            let t' = I1.create ~len:(length + 1) data in
            let t' =
              I1.unsafe_blit ~src:t ~dst:t' ~src_pos:0 ~dst_pos:1 ~len:length
            in
            t'
          with Exit -> t))
  | Value1 -> (
      let exception Exit in
      try
        let length = L1.length t in
        for i = 0 to length - 1 do
          let x = L1.unsafe_get t i in
          let key' = unpack decoder x in
          match key_equal key key' with
          | true ->
              L1.unsafe_set t i data;
              raise_notrace Exit
          | false -> ()
        done;
        (* Key was not found: add it to the start *)
        let t' = L1.unsafe_create_uninitialized ~len:(length + 1) in
        L1.unsafe_blit ~src:t ~dst:t' ~src_pos:0 ~dst_pos:1 ~len:length;
        L1.unsafe_set t' 0 data;
        t'
      with Exit -> t)
  | Value2 -> (
      let data1, data2 = data in
      let exception Exit in
      try
        let length = L2.length t in
        for i = 0 to length - 1 do
          let x = L2.unsafe_get t i in
          let key' = unpack decoder x in
          match key_equal key key' with
          | true ->
              L2.unsafe_set t i data1 data2;
              raise_notrace Exit
          | false -> ()
        done;
        (* Key was not found: add it to the start *)
        let t' = L2.unsafe_create_uninitialized ~len:(length + 1) in
        L2.unsafe_blit ~src:t ~dst:t' ~src_pos:0 ~dst_pos:1 ~len:length;
        L2.unsafe_set t' 0 data1 data2;
        t'
      with Exit -> t)
  | Value3 -> assert false

let cons : type a b. (a, b) Entry_size.t -> a -> b -> b =
 fun entry_size x xs ->
  match entry_size with
  | Immediate ->
      let old_len = I1.length xs in
      let len = old_len + 1 in
      let arr = I1.create ~len x in
      let arr =
        I1.unsafe_blit ~src:xs ~dst:arr ~src_pos:0 ~dst_pos:1 ~len:old_len
      in
      arr
  | Value1 ->
      let old_len = L1.length xs in
      let len = old_len + 1 in
      let arr = L1.unsafe_create_uninitialized ~len in
      L1.unsafe_blit ~src:xs ~dst:arr ~src_pos:0 ~dst_pos:1 ~len:old_len;
      L1.unsafe_set arr 0 x;
      arr
  | Value2 ->
      let x, y = x in
      let old_len = L2.length xs in
      let len = old_len + 1 in
      let arr = L2.unsafe_create_uninitialized ~len in
      L2.unsafe_blit ~src:xs ~dst:arr ~src_pos:0 ~dst_pos:1 ~len:old_len;
      L2.unsafe_set arr 0 x y;
      arr
  | Value3 ->
      let x, y, z = x in
      let old_len = L3.length xs in
      let len = old_len + 1 in
      let arr = L3.unsafe_create_uninitialized ~len in
      L3.unsafe_blit ~src:xs ~dst:arr ~src_pos:0 ~dst_pos:1 ~len:old_len;
      L3.unsafe_set arr 0 x y z;
      arr

let exists : type a b. (a, b) Entry_size.t -> f:(a -> bool) -> b -> bool =
 fun entry_size ~f t ->
  match entry_size with
  | Immediate -> I1.exists t ~f
  | Value1 -> L1.exists t ~f
  | Value2 -> L2.exists t ~f:(curry2 f)
  | Value3 -> L3.exists t ~f:(curry3 f)

let find_map :
    type a b r. (a, b) Entry_size.t -> f:(a -> r option) -> b -> r option =
 fun entry_size ~f t ->
  match entry_size with
  | Immediate ->
      let rec aux ~f ~len t i =
        if i = len then None
        else
          match f (I1.unsafe_get t i) with
          | Some _ as r -> r
          | None -> aux ~f ~len t (i + 1)
      in
      aux ~f ~len:(I1.length t) t 0
  | Value1 ->
      let rec aux ~f ~len t i =
        if i = len then None
        else
          match f (L1.unsafe_get t i) with
          | Some _ as r -> r
          | None -> aux ~f ~len t (i + 1)
      in
      aux ~f ~len:(L1.length t) t 0
  | Value2 ->
      let rec aux ~f ~len t i =
        if i = len then None
        else
          match f (L2.unsafe_get_fst t i, L2.unsafe_get_snd t i) with
          | Some _ as r -> r
          | None -> aux ~f ~len t (i + 1)
      in
      aux ~f ~len:(L2.length t) t 0
  | Value3 ->
      let rec aux ~f ~len t i =
        if i = len then None
        else
          match
            f
              ( L3.unsafe_get_fst t i
              , L3.unsafe_get_snd t i
              , L3.unsafe_get_thd t i )
          with
          | Some _ as r -> r
          | None -> aux ~f ~len t (i + 1)
      in
      aux ~f ~len:(L3.length t) t 0

let fold_left :
    type a b acc.
    (a, b) Entry_size.t -> f:(acc -> a -> acc) -> init:acc -> b -> acc =
 fun entry_size ~f ~init t ->
  match entry_size with
  | Immediate -> I1.fold t ~init ~f
  | Value1 -> L1.fold t ~init ~f
  | Value2 -> L2.fold t ~init ~f:(fun acc a b -> f acc (a, b))
  | Value3 -> L3.fold t ~init ~f:(fun acc a b c -> f acc (a, b, c))

let iter : type a b. (a, b) Entry_size.t -> f:(a -> unit) -> b -> unit =
 fun entry_size ~f t ->
  match entry_size with
  | Immediate -> I1.iter ~f t
  | Value1 -> L1.iter ~f t
  | Value2 -> L2.iter ~f:(curry2 f) t
  | Value3 -> L3.iter ~f:(curry3 f) t

let to_array : type a b. (a, b) Entry_size.t -> b -> a array =
 fun entry_size t ->
  match entry_size with
  | Immediate -> I1.to_array t
  | Value1 -> L1.to_array t
  | Value2 -> L2.to_array t
  | Value3 -> L3.to_array t

let of_list_rev : type a b. (a, b) Entry_size.t -> a list -> b =
 fun entry_size t ->
  match entry_size with
  | Immediate -> I1.of_list_rev t
  | Value1 -> L1.of_list_rev t
  | Value2 -> L2.of_list_rev t
  | Value3 -> L3.of_list_rev t

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
