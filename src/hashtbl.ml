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

let key_impl_of_key :
    type k v.
    (module Key with type t = k) -> (k, unit, k * v) Hashed_container.key_impl =
 fun (module Key) ->
  (module struct
    include Key

    type packed = Key.t * v
    type decoder = unit

    let unpack () (k, _) = k
  end)

let entry_impl_of_key :
    type k v.
       (module Key with type t = k)
    -> (k, v, k * v, unit, k * v) Hashed_container.entry_impl =
 fun (module Key) ->
  (module struct
    type t = Key.t * v
    type key = Key.t
    type packed = Key.t * v
    type nonrec value = v
    type decoder = unit

    let key (k, _) = k
    let value (_, v) = v
    let pack () t = t
    let unpack () t = t
    let compare = Stdlib.compare (* XXX: polymorphic comparison *)
  end)

let map : type k v1 v2. (k, v1) t -> f:(v1 -> v2) -> (k, v2) t =
 fun t ~f ->
  let key = (module (val Hashed_container.key_impl t) : Key with type t = k) in
  let key_impl : (k, unit, k * v2) Hashed_container.key_impl =
    key_impl_of_key key
  in
  let entry_impl : (k, v2, k * v2, unit, k * v2) Hashed_container.entry_impl =
    entry_impl_of_key key
  in
  map_poly t ~key_impl ~entry_impl ~f:(fun (key, data) -> (key, f data))

let entry_size = Hashed_container.Entry_size.Value2

let create ~initial_capacity (type key value)
    (key : (module Key with type t = key)) : (key, value) t =
  Hashed_container.create ~initial_capacity ~key:(key_impl_of_key key)
    ~entry:(entry_impl_of_key key) ~entry_size ()

let create_poly ~initial_capacity () (type key value) : (key, value) t =
  let key : (module Key with type t = key) =
    (module struct
      type t = key

      let equal = ( = )
      let hash = Stdlib.Hashtbl.hash
      let hash_size = 30
    end)
  in
  Hashed_container.create ~initial_capacity ~key:(key_impl_of_key key)
    ~entry:(entry_impl_of_key key) ~entry_size ()

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
