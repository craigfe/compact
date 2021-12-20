(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Hashed_container.No_decoder

type nonrec ('k, 'v) t = ('k, 'v, 'k * 'v, 'k * 'v) t

let replace t k v = replace t k (k, v)
let iter t ~f = iter t ~f:(fun (k, v) -> f k v)
let fold t ~f ~init = fold t ~f:(fun acc (k, v) -> f acc k v) ~init
let count t ~f = count t ~f:(fun (k, v) -> f k v)

module type Key = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val hash_size : int
end

let create ~initial_capacity (type key value)
    (module Key : Key with type t = key) : (key, value) t =
  let module Key = struct
    include Key

    type packed = Key.t * value
    type decoder = unit

    let unpack () (k, _) = k
  end in
  let module Entry = struct
    type t = Key.t * value
    type key = Key.t
    type packed = Key.t * value
    type nonrec value = value
    type decoder = unit

    let key (k, _) = k
    let value (_, v) = v
    let unpack () t = t
    let compare = Stdlib.compare (* XXX: polymorphic comparison *)
  end in
  Hashed_container.create ~initial_capacity
    ~key:(module Key)
    ~entry:(module Entry)
    ~entry_size:Hashed_container.Entry_size.Value2 ()

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
