(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2016–2020 Jane Street Group, LLC <opensource@janestreet.com>
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(** Code in this module has been extracted from Jane Street's [base] library,
    and modified to add support for [Tuple2] and [Tuple3] specialisations. *)

open! Import
include Uniform_array_intf

module type Trusted = sig
  type ('a, 'b, 'c) t
  type ('a, 'b, 'c) elt
  type ('a, 'b, 'c, 'inner) with_elt

  val empty : (_, _, _) t
  val unsafe_create_uninitialized : len:int -> (_, _, _) t
  val create_obj_array : len:int -> (_, _, _) t
  val create : len:int -> ('a, 'b, 'c, ('a, 'b, 'c) t) with_elt
  val singleton : ('a, 'b, 'c, ('a, 'b, 'c) t) with_elt
  val get : ('a, 'b, 'c) t -> int -> ('a, 'b, 'c) elt
  val set : ('a, 'b, 'c) t -> int -> ('a, 'b, 'c, unit) with_elt
  val swap : (_, _, _) t -> int -> int -> unit
  val unsafe_get : ('a, 'b, 'c) t -> int -> ('a, 'b, 'c) elt
  val unsafe_set : ('a, 'b, 'c) t -> int -> ('a, 'b, 'c, unit) with_elt

  val unsafe_set_omit_phys_equal_check :
    ('a, 'b, 'c) t -> int -> ('a, 'b, 'c, unit) with_elt

  val unsafe_set_int : (_, _, _) t -> int -> (int, int, int, unit) with_elt

  val unsafe_set_int_assuming_currently_int :
    (_, _, _) t -> int -> (int, int, int, unit) with_elt

  val unsafe_set_assuming_currently_int :
    (_, _, _) t -> int -> (Obj.t, Obj.t, Obj.t, unit) with_elt

  val length : (_, _, _) t -> int

  val unsafe_blit :
       src:('a, 'b, 'c) t
    -> src_pos:int
    -> dst:('a, 'b, 'c) t
    -> dst_pos:int
    -> len:int
    -> unit

  val unsafe_clear_if_pointer : (_, _, _) t -> int -> unit
end

(* WARNING:
   We use non-memory-safe things throughout the [Trusted] module.
   Most of it is only safe in combination with the type signature (e.g. exposing
   [val copy : 'a t -> 'b t] would be a big mistake). *)
module Trusted : sig
  type 'a t

  include
    Trusted
      with type ('a, _, _) t := 'a t
       and type ('a, _, _) elt := 'a
       and type ('a, _, _, 'inner) with_elt := 'a -> 'inner
end = struct
  type 'a t = Obj_array.t

  let empty = Obj_array.empty
  let unsafe_create_uninitialized ~len = Obj_array.create_zero ~len
  let create_obj_array ~len = Obj_array.create_zero ~len
  let create ~len x = Obj_array.create ~len (Obj.repr x)
  let singleton x = Obj_array.singleton (Obj.repr x)
  let swap t i j = Obj_array.swap t i j
  let get arr i = Obj.obj (Obj_array.get arr i)
  let set arr i x = Obj_array.set arr i (Obj.repr x)
  let unsafe_get arr i = Obj.obj (Obj_array.unsafe_get arr i)
  let unsafe_set arr i x = Obj_array.unsafe_set arr i (Obj.repr x)
  let unsafe_set_int arr i x = Obj_array.unsafe_set_int arr i x

  let unsafe_set_int_assuming_currently_int arr i x =
    Obj_array.unsafe_set_int_assuming_currently_int arr i x

  let unsafe_set_assuming_currently_int arr i x =
    Obj_array.unsafe_set_assuming_currently_int arr i (Obj.repr x)

  let length = Obj_array.length
  let unsafe_blit = Obj_array.unsafe_blit

  let unsafe_set_omit_phys_equal_check t i x =
    Obj_array.unsafe_set_omit_phys_equal_check t i (Obj.repr x)

  let unsafe_clear_if_pointer = Obj_array.unsafe_clear_if_pointer
end

include Trusted

let init l ~f =
  if l < 0 then invalid_arg "Uniform_array.init"
  else
    let res = unsafe_create_uninitialized ~len:l in
    for i = 0 to l - 1 do
      unsafe_set res i (f i)
    done;
    res

let of_array arr = init ~f:(Array.unsafe_get arr) (Array.length arr)
let map a ~f = init ~f:(fun i -> f (unsafe_get a i)) (length a)

let map_inplace a ~f =
  for i = 0 to length a - 1 do
    unsafe_set a i (f (unsafe_get a i))
  done

let iter a ~f =
  for i = 0 to length a - 1 do
    f (unsafe_get a i)
  done

let iteri a ~f =
  for i = 0 to length a - 1 do
    f i (unsafe_get a i)
  done

let invariant inv_a t =
  assert (Obj.tag (Obj.repr t) <> Obj.double_array_tag);
  iter t ~f:inv_a

let to_list t = List.init ~f:(get t) ~len:(length t)

let of_list l =
  let len = List.length l in
  let res = unsafe_create_uninitialized ~len in
  List.iteri l ~f:(fun i x -> set res i x);
  res

let of_list_rev l =
  match l with
  | [] -> empty
  | a :: l ->
      let len = 1 + List.length l in
      let t = create ~len a in
      let r = ref l in
      (* We start at [len - 2] because we already put [a] at [t.(len - 1)]. *)
      for i = len - 2 downto 0 do
        match !r with
        | [] -> assert false
        | a :: l ->
            unsafe_set t i a;
            r := l
      done;
      t

(* It is not safe for [to_array] to be the identity function because we have code that
   relies on [float array]s being unboxed, for example in [bin_write_array]. *)
let to_array t = Array.init (length t) ~f:(fun i -> unsafe_get t i)

let exists t ~f =
  let rec loop t ~f i =
    if i < 0 then false else f (unsafe_get t i) || loop t ~f (i - 1)
  in
  loop t ~f (length t - 1)

let map2_exn t1 t2 ~f =
  let len = length t1 in
  if length t2 <> len then invalid_arg "Array.map2_exn";
  init len ~f:(fun i -> f (unsafe_get t1 i) (unsafe_get t2 i))

let fold t ~init ~f =
  let r = ref init in
  for i = 0 to length t - 1 do
    r := f !r (unsafe_get t i)
  done;
  !r

module Tuple2 = struct
  (** See {!Trusted} above. *)
  module Trusted : sig
    type ('a, 'b) t

    include
      Trusted
        with type ('a, 'b, _) t := ('a, 'b) t
         and type ('a, 'b, _) elt := 'a * 'b
         and type ('a, 'b, _, 'inner) with_elt := 'a -> 'b -> 'inner

    val get_fst : ('a, _) t -> int -> 'a
    val get_snd : (_, 'b) t -> int -> 'b
    val set_fst : ('a, _) t -> int -> 'a -> unit
    val set_snd : (_, 'b) t -> int -> 'b -> unit
    val unsafe_get_fst : ('a, _) t -> int -> 'a
    val unsafe_get_snd : (_, 'b) t -> int -> 'b
    val unsafe_set_fst : ('a, _) t -> int -> 'a -> unit
    val unsafe_set_snd : (_, 'b) t -> int -> 'b -> unit
  end = struct
    type ('a, 'b) t = Obj_array.t

    let entry_size = 2
    let empty = Obj_array.empty

    let unsafe_create_uninitialized ~len =
      Obj_array.create_zero ~len:(len * entry_size)

    let create_obj_array ~len = Obj_array.create_zero ~len:(len * entry_size)

    let create ~len x y =
      let t = unsafe_create_uninitialized ~len:(len * entry_size) in
      for i = 0 to len - 1 do
        Obj_array.unsafe_set t (entry_size * i) (Obj.repr x);
        Obj_array.unsafe_set t ((entry_size * i) + 1) (Obj.repr y)
      done;
      t

    let singleton x y = create ~len:1 x y

    let swap t i j =
      Obj_array.swap t (entry_size * i) (entry_size * j);
      Obj_array.swap t ((entry_size * i) + 1) ((entry_size * j) + 1)

    let get_fst arr i = Obj.obj (Obj_array.get arr (entry_size * i))
    let get_snd arr i = Obj.obj (Obj_array.get arr ((entry_size * i) + 1))
    let get arr i = (get_fst arr i, get_snd arr i)
    let set_fst arr i x = Obj_array.set arr (entry_size * i) (Obj.repr x)
    let set_snd arr i y = Obj_array.set arr ((entry_size * i) + 1) (Obj.repr y)

    let set arr i x y =
      set_fst arr i x;
      set_snd arr i y

    let unsafe_get_fst arr i =
      Obj.obj (Obj_array.unsafe_get arr (entry_size * i))

    let unsafe_get_snd arr i =
      Obj.obj (Obj_array.unsafe_get arr ((entry_size * i) + 1))

    let unsafe_get arr i = (unsafe_get_fst arr i, unsafe_get_snd arr i)

    let unsafe_set_fst arr i x =
      Obj_array.unsafe_set arr (entry_size * i) (Obj.repr x)

    let unsafe_set_snd arr i y =
      Obj_array.unsafe_set arr ((entry_size * i) + 1) (Obj.repr y)

    let unsafe_set arr i x y =
      unsafe_set_fst arr i x;
      unsafe_set_snd arr i y

    let length t = Obj_array.length t / entry_size

    let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
      Obj_array.unsafe_blit ~src ~src_pos:(entry_size * src_pos) ~dst
        ~dst_pos:(entry_size * dst_pos) ~len:(entry_size * len)

    let unsafe_set_omit_phys_equal_check t i x y =
      Obj_array.unsafe_set_omit_phys_equal_check t (entry_size * i) (Obj.repr x);
      Obj_array.unsafe_set_omit_phys_equal_check t
        ((entry_size * i) + 1)
        (Obj.repr y)

    let unsafe_set_int arr i x y =
      Obj_array.unsafe_set_int arr (entry_size * i) x;
      Obj_array.unsafe_set_int arr ((entry_size * i) + 1) y

    let unsafe_set_int_assuming_currently_int arr i x y =
      Obj_array.unsafe_set_int_assuming_currently_int arr (entry_size * i) x;
      Obj_array.unsafe_set_int_assuming_currently_int arr
        ((entry_size * i) + 1)
        y

    let unsafe_set_assuming_currently_int arr i x y =
      Obj_array.unsafe_set_assuming_currently_int arr (entry_size * i) x;
      Obj_array.unsafe_set_assuming_currently_int arr ((entry_size * i) + 1) y

    let unsafe_clear_if_pointer arr i =
      Obj_array.unsafe_clear_if_pointer arr (entry_size * i);
      Obj_array.unsafe_clear_if_pointer arr ((entry_size * i) + 1)
  end

  include Trusted

  let init l ~f =
    if l < 0 then invalid_arg "Uniform_array.init"
    else
      let res = unsafe_create_uninitialized ~len:l in
      for i = 0 to l - 1 do
        let x, y = f i in
        unsafe_set res i x y
      done;
      res

  let of_array arr = init ~f:(Array.unsafe_get arr) (Array.length arr)

  let map a ~f =
    init ~f:(fun i -> f (unsafe_get_fst a i) (unsafe_get_snd a i)) (length a)

  let map_inplace a ~f =
    for i = 0 to length a - 1 do
      let x, y = f (unsafe_get_fst a i) (unsafe_get_snd a i) in
      unsafe_set a i x y
    done

  let iter a ~f =
    for i = 0 to length a - 1 do
      f (unsafe_get_fst a i) (unsafe_get_snd a i)
    done

  let iteri a ~f =
    for i = 0 to length a - 1 do
      f i (unsafe_get_fst a i) (unsafe_get_snd a i)
    done

  let invariant inv_elt t =
    assert (Obj.tag (Obj.repr t) <> Obj.double_array_tag);
    iter t ~f:(fun a b -> inv_elt (a, b))

  let to_list t = List.init ~f:(get t) ~len:(length t)

  let of_list l =
    let len = List.length l in
    let res = unsafe_create_uninitialized ~len in
    List.iteri l ~f:(fun i (x, y) -> unsafe_set res i x y);
    res

  let of_list_rev l =
    match l with
    | [] -> empty
    | (a, b) :: l ->
        let len = 1 + List.length l in
        let t = unsafe_create_uninitialized ~len in
        unsafe_set_fst t (len - 1) a;
        unsafe_set_snd t (len - 1) b;
        let r = ref l in
        (* We start at [len - 2] because we already put [a] at [t.(len - 1)]. *)
        for i = len - 2 downto 0 do
          match !r with
          | [] -> assert false
          | (a, b) :: l ->
              unsafe_set_fst t i a;
              unsafe_set_snd t i b;
              r := l
        done;
        t

  (* It is not safe for [to_array] to be the identity function because we have code that
     relies on [float array]s being unboxed, for example in [bin_write_array]. *)
  let to_array t = Array.init (length t) ~f:(fun i -> unsafe_get t i)

  let exists t ~f =
    let rec loop t ~f i =
      if i < 0 then false
      else f (unsafe_get_fst t i) (unsafe_get_snd t i) || loop t ~f (i - 1)
    in
    loop t ~f (length t - 1)

  let map2_exn t1 t2 ~f =
    let len = length t1 in
    if length t2 <> len then invalid_arg "Array.map2_exn";
    init len ~f:(fun i ->
        f (unsafe_get_fst t1 i) (unsafe_get_snd t1 i) (unsafe_get_fst t2 i)
          (unsafe_get_snd t2 i))

  let fold t ~init ~f =
    let r = ref init in
    for i = 0 to length t - 1 do
      r := f !r (unsafe_get_fst t i) (unsafe_get_snd t i)
    done;
    !r
end

module Tuple3 = struct
  (** See {!Trusted} above. *)
  module Trusted : sig
    type ('a, 'b, 'c) t

    include
      Trusted
        with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
         and type ('a, 'b, 'c) elt := 'a * 'b * 'c
         and type ('a, 'b, 'c, 'inner) with_elt := 'a -> 'b -> 'c -> 'inner

    val get_fst : ('a, _, _) t -> int -> 'a
    val get_snd : (_, 'b, _) t -> int -> 'b
    val get_thd : (_, _, 'c) t -> int -> 'c
    val set_fst : ('a, _, _) t -> int -> 'a -> unit
    val set_snd : (_, 'b, _) t -> int -> 'b -> unit
    val set_thd : (_, _, 'c) t -> int -> 'c -> unit
    val unsafe_get_fst : ('a, _, _) t -> int -> 'a
    val unsafe_get_snd : (_, 'b, _) t -> int -> 'b
    val unsafe_get_thd : (_, _, 'c) t -> int -> 'c
    val unsafe_set_fst : ('a, _, _) t -> int -> 'a -> unit
    val unsafe_set_snd : (_, 'b, _) t -> int -> 'b -> unit
    val unsafe_set_thd : (_, _, 'c) t -> int -> 'c -> unit
  end = struct
    type ('a, 'b, 'c) t = Obj_array.t

    let entry_size = 3
    let empty = Obj_array.empty

    let unsafe_create_uninitialized ~len =
      Obj_array.create_zero ~len:(len * entry_size)

    let create_obj_array ~len = Obj_array.create_zero ~len:(len * entry_size)

    let create ~len x y z =
      let t = unsafe_create_uninitialized ~len:(len * entry_size) in
      for i = 0 to len - 1 do
        Obj_array.unsafe_set t (entry_size * i) (Obj.repr x);
        Obj_array.unsafe_set t ((entry_size * i) + 1) (Obj.repr y);
        Obj_array.unsafe_set t ((entry_size * i) + 2) (Obj.repr z)
      done;
      t

    let singleton x y = create ~len:1 x y

    let swap t i j =
      Obj_array.swap t (entry_size * i) (entry_size * j);
      Obj_array.swap t ((entry_size * i) + 1) ((entry_size * j) + 1);
      Obj_array.swap t ((entry_size * i) + 2) ((entry_size * j) + 2)

    let get_fst arr i = Obj.obj (Obj_array.get arr (entry_size * i))
    let get_snd arr i = Obj.obj (Obj_array.get arr ((entry_size * i) + 1))
    let get_thd arr i = Obj.obj (Obj_array.get arr ((entry_size * i) + 2))
    let get arr i = (get_fst arr i, get_snd arr i, get_thd arr i)
    let set_fst arr i x = Obj_array.set arr (entry_size * i) (Obj.repr x)
    let set_snd arr i y = Obj_array.set arr ((entry_size * i) + 1) (Obj.repr y)
    let set_thd arr i z = Obj_array.set arr ((entry_size * i) + 2) (Obj.repr z)

    let set arr i x y z =
      set_fst arr i x;
      set_snd arr i y;
      set_thd arr i z

    let unsafe_get_fst arr i =
      Obj.obj (Obj_array.unsafe_get arr (entry_size * i))

    let unsafe_get_snd arr i =
      Obj.obj (Obj_array.unsafe_get arr ((entry_size * i) + 1))

    let unsafe_get_thd arr i =
      Obj.obj (Obj_array.unsafe_get arr ((entry_size * i) + 2))

    let unsafe_get arr i =
      (unsafe_get_fst arr i, unsafe_get_snd arr i, unsafe_get_thd arr i)

    let unsafe_set_fst arr i x =
      Obj_array.unsafe_set arr (entry_size * i) (Obj.repr x)

    let unsafe_set_snd arr i y =
      Obj_array.unsafe_set arr ((entry_size * i) + 1) (Obj.repr y)

    let unsafe_set_thd arr i z =
      Obj_array.unsafe_set arr ((entry_size * i) + 2) (Obj.repr z)

    let unsafe_set arr i x y z =
      unsafe_set_fst arr i x;
      unsafe_set_snd arr i y;
      unsafe_set_thd arr i z

    let length t = Obj_array.length t / entry_size

    let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
      Obj_array.unsafe_blit ~src ~src_pos:(entry_size * src_pos) ~dst
        ~dst_pos:(entry_size * dst_pos) ~len:(entry_size * len)

    let unsafe_set_omit_phys_equal_check t i x y z =
      Obj_array.unsafe_set_omit_phys_equal_check t (entry_size * i) (Obj.repr x);
      Obj_array.unsafe_set_omit_phys_equal_check t
        ((entry_size * i) + 1)
        (Obj.repr y);
      Obj_array.unsafe_set_omit_phys_equal_check t
        ((entry_size * i) + 2)
        (Obj.repr z)

    let unsafe_set_int arr i x y z =
      Obj_array.unsafe_set_int arr (entry_size * i) x;
      Obj_array.unsafe_set_int arr ((entry_size * i) + 1) y;
      Obj_array.unsafe_set_int arr ((entry_size * i) + 2) z

    let unsafe_set_int_assuming_currently_int arr i x y z =
      Obj_array.unsafe_set_int_assuming_currently_int arr (entry_size * i) x;
      Obj_array.unsafe_set_int_assuming_currently_int arr
        ((entry_size * i) + 1)
        y;
      Obj_array.unsafe_set_int_assuming_currently_int arr
        ((entry_size * i) + 2)
        z

    let unsafe_set_assuming_currently_int arr i x y z =
      Obj_array.unsafe_set_assuming_currently_int arr (entry_size * i) x;
      Obj_array.unsafe_set_assuming_currently_int arr ((entry_size * i) + 1) y;
      Obj_array.unsafe_set_assuming_currently_int arr ((entry_size * i) + 2) z

    let unsafe_clear_if_pointer arr i =
      Obj_array.unsafe_clear_if_pointer arr (entry_size * i);
      Obj_array.unsafe_clear_if_pointer arr ((entry_size * i) + 1);
      Obj_array.unsafe_clear_if_pointer arr ((entry_size * i) + 2)
  end

  include Trusted

  let init l ~f =
    if l < 0 then invalid_arg "Uniform_array.init"
    else
      let res = unsafe_create_uninitialized ~len:l in
      for i = 0 to l - 1 do
        let x, y, z = f i in
        unsafe_set res i x y z
      done;
      res

  let of_array arr = init ~f:(Array.unsafe_get arr) (Array.length arr)

  let map a ~f =
    init
      ~f:(fun i ->
        f (unsafe_get_fst a i) (unsafe_get_snd a i) (unsafe_get_thd a i))
      (length a)

  let map_inplace a ~f =
    for i = 0 to length a - 1 do
      let x, y, z =
        f (unsafe_get_fst a i) (unsafe_get_snd a i) (unsafe_get_thd a i)
      in
      unsafe_set a i x y z
    done

  let iter a ~f =
    for i = 0 to length a - 1 do
      f (unsafe_get_fst a i) (unsafe_get_snd a i) (unsafe_get_thd a i)
    done

  let iteri a ~f =
    for i = 0 to length a - 1 do
      f i (unsafe_get_fst a i) (unsafe_get_snd a i) (unsafe_get_thd a i)
    done

  let invariant inv_elt t =
    assert (Obj.tag (Obj.repr t) <> Obj.double_array_tag);
    iter t ~f:(fun a b c -> inv_elt (a, b, c))

  let to_list t = List.init ~f:(get t) ~len:(length t)

  let of_list l =
    let len = List.length l in
    let res = unsafe_create_uninitialized ~len in
    List.iteri l ~f:(fun i (x, y, z) -> unsafe_set res i x y z);
    res

  let of_list_rev l =
    match l with
    | [] -> empty
    | (a, b, c) :: l ->
        let len = 1 + List.length l in
        let t = unsafe_create_uninitialized ~len in
        unsafe_set_fst t (len - 1) a;
        unsafe_set_snd t (len - 1) b;
        unsafe_set_thd t (len - 1) c;
        let r = ref l in
        (* We start at [len - 2] because we already put [a] at [t.(len - 1)]. *)
        for i = len - 2 downto 0 do
          match !r with
          | [] -> assert false
          | (a, b, c) :: l ->
              unsafe_set_fst t i a;
              unsafe_set_snd t i b;
              unsafe_set_thd t i c;
              r := l
        done;
        t

  (* It is not safe for [to_array] to be the identity function because we have code that
     relies on [float array]s being unboxed, for example in [bin_write_array]. *)
  let to_array t = Array.init (length t) ~f:(fun i -> unsafe_get t i)

  let exists t ~f =
    let rec loop t ~f i =
      if i < 0 then false
      else
        f (unsafe_get_fst t i) (unsafe_get_snd t i) (unsafe_get_thd t i)
        || loop t ~f (i - 1)
    in
    loop t ~f (length t - 1)

  let map2_exn t1 t2 ~f =
    let len = length t1 in
    if length t2 <> len then invalid_arg "Array.map2_exn";
    init len ~f:(fun i ->
        f (unsafe_get_fst t1 i) (unsafe_get_snd t1 i) (unsafe_get_thd t1 i)
          (unsafe_get_fst t2 i) (unsafe_get_snd t2 i) (unsafe_get_thd t2 i))

  let fold t ~init ~f =
    let r = ref init in
    for i = 0 to length t - 1 do
      r := f !r (unsafe_get_fst t i) (unsafe_get_snd t i) (unsafe_get_thd t i)
    done;
    !r
end

(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2016–2020 Jane Street Group, LLC <opensource@janestreet.com>
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
