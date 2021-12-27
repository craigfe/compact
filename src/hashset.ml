(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Hashed_container.No_decoder

type nonrec 'a t = ('a, unit, 'a, 'a) t

let add t k = replace t k k

module type Key = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val hash_size : int
end

let key_impl_of_key :
    type k.
    (module Key with type t = k) -> (k, unit, k) Hashed_container.key_impl =
 fun (module Key) ->
  (module struct
    include Key

    type packed = Key.t
    type decoder = unit

    let unpack () t = t
  end)

let entry_impl_of_key :
    type k.
       (module Key with type t = k)
    -> (k, unit, k, unit, k) Hashed_container.entry_impl =
 fun (module Key) ->
  (module struct
    type t = Key.t
    type key = Key.t
    type packed = Key.t
    type value = unit
    type decoder = unit

    let key t = t
    let value (_ : t) = ()
    let pack () t = t
    let unpack () t = t
    let compare = Stdlib.compare (* XXX: polymorphic comparison *)
  end)

let create_generic (type a) ~(entry_size : (a, _) Hashed_container.Entry_size.t)
    ~initial_capacity (key : (module Key with type t = a)) : a t =
  Hashed_container.create ~initial_capacity ~key:(key_impl_of_key key)
    ~entry:(entry_impl_of_key key) ~entry_size ()

let create (type a) ~initial_capacity (module Key : Key with type t = a) : a t =
  create_generic ~entry_size:Value1 ~initial_capacity (module Key)

module Immediate = struct
  include Hashed_container.No_decoder

  type nonrec 'a t = ('a, unit, 'a, 'a) t

  let add t k = replace t k k
  let entry_size = Hashed_container.Entry_size.Immediate

  let create (type a) ~initial_capacity (module Key : Key with type t = a) : a t
      =
    create_generic ~entry_size ~initial_capacity (module Key)
end

module Int = struct
  include Immediate

  type nonrec t = int t

  module Key = struct
    include Stdlib.Int

    let hash = Stdlib.Hashtbl.hash
    let hash_size = 30
  end

  let create ~initial_capacity () : t =
    create_generic ~entry_size:Immediate.entry_size ~initial_capacity
      (module Key)
end

module Immediate64 = struct
  include Hashed_container.No_decoder

  type nonrec 'a t = ('a, unit, 'a, 'a) t

  let add t k = replace t k k

  type _ boxed_entry_size =
    | E : ('a, _) Hashed_container.Entry_size.t -> 'a boxed_entry_size
  [@@unboxed]

  let entry_size = if Sys.word_size = 64 then E Immediate else E Value1

  let create (type a) ~initial_capacity (module Key : Key with type t = a) : a t
      =
    let (E entry_size) = entry_size in
    create_generic ~entry_size ~initial_capacity (module Key)
end

module Fixed_size_string = Hashset_fixed_size_string

module Internal = struct
  type nonrec 'a t = 'a t

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
