(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Hashed_container.No_decoder

type nonrec ('k, 'v) t = ('k, 'v, 'k * 'v, 'k * 'v) t

let replace t ~key ~data = replace t key (key, data)
let add t ~key ~data = add t key (key, data)
let add_exn t ~key ~data = add_exn t key (key, data)
let iter t ~f = iter t ~f:(fun (key, data) -> f ~key ~data)
let map_inplace t ~f = map_inplace t ~f:(fun (key, data) -> (key, f data))
let fold t ~f ~init = fold t ~f:(fun acc (key, data) -> f acc ~key ~data) ~init
let exists t ~f = exists t ~f:(fun (key, data) -> f ~key ~data)
let for_all t ~f = for_all t ~f:(fun (key, data) -> f ~key ~data)
let count t ~f = count t ~f:(fun (key, data) -> f ~key ~data)

module type Key = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val hash_size : int
end

let vtable_of_key :
    type k v.
       (module Key with type t = k)
    -> (k, v, k * v, unit, k * v) Hashed_container.vtable =
 fun (module Key) ->
  { key_hash = Key.hash
  ; key_hash_size = Key.hash_size
  ; key_equal = Key.equal
  ; entry_key = (fun (k, _) -> k)
  ; entry_value = (fun (_, v) -> v)
  ; entry_compare = Stdlib.compare (* XXX: polymorphic comparison *)
  ; packed_key = (fun () (k, _) -> k)
  ; packed_entry = (fun () kv -> kv)
  ; packed_of_entry = (fun () kv -> kv)
  }

let map : type k v1 v2. (k, v1) t -> f:(v1 -> v2) -> (k, v2) t =
 fun t ~f ->
  let vtable = Hashed_container.vtable t in
  let key : (module Key with type t = k) =
    (module struct
      type t = k

      let equal = vtable.key_equal
      let hash = vtable.key_hash
      let hash_size = vtable.key_hash_size
    end)
  in
  map_poly t ~vtable:(vtable_of_key key) ~f:(fun (key, data) -> (key, f data))

let entry_size = Hashed_container.Entry_size.Value2

let create ~initial_capacity (type key value)
    (key : (module Key with type t = key)) : (key, value) t =
  Hashed_container.create ~initial_capacity ~vtable:(vtable_of_key key)
    ~entry_size ()

let create_poly ~initial_capacity () (type key value) : (key, value) t =
  let key : (module Key with type t = key) =
    (module struct
      type t = key

      let equal = ( = )
      let hash = Stdlib.Hashtbl.hash
      let hash_size = 30
    end)
  in
  Hashed_container.create ~initial_capacity ~vtable:(vtable_of_key key)
    ~entry_size ()

module Internal = struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let repr = Type_equality.Refl
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
