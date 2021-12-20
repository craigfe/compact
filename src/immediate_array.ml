open! Import

type 'a t = ('a, 'a array) Obj_either.t

let to_array t =
  match Obj_either.inspect t with
  | Immediate -> [| Obj_either.get_immediate_exn t |]
  | Value -> Obj_either.get_value_exn t

let of_list = function
  | [ x ] -> Obj_either.of_immediate x
  | l -> Obj_either.of_value (Array.of_list l)

let of_list_rev = function
  | [ x ] -> Obj_either.of_immediate x
  | [] -> Obj_either.of_value [||]
  | a :: l ->
      let len = 1 + List.length l in
      let arr = Array.make len a in
      let r = ref l in
      (* We start at [len - 2] because we already put [a] at [t.(len - 1)]. *)
      for i = len - 2 downto 0 do
        match !r with
        | [] -> assert false
        | a :: l ->
            Array.unsafe_set arr i a;
            r := l
      done;
      Obj_either.of_value arr

let to_list t =
  match Obj_either.inspect t with
  | Immediate -> [ Obj_either.get_immediate_exn t ]
  | Value -> Array.to_list (Obj_either.get_value_exn t)

let iter t ~f =
  match Obj_either.inspect t with
  | Immediate -> f (Obj_either.get_immediate_exn t)
  | Value -> Array.iter ~f (Obj_either.get_value_exn t)

let fold t ~f ~init =
  match Obj_either.inspect t with
  | Immediate -> f init (Obj_either.get_immediate_exn t)
  | Value -> Array.fold_left ~f ~init (Obj_either.get_value_exn t)

let exists t ~f =
  match Obj_either.inspect t with
  | Immediate -> f (Obj_either.get_immediate_exn t)
  | Value -> Array.exists ~f (Obj_either.get_value_exn t)

let length t =
  match Obj_either.inspect t with
  | Immediate -> 1
  | Value -> Array.length (Obj_either.get_value_exn t)

let unsafe_get t i =
  match Obj_either.inspect t with
  | Immediate -> Obj_either.get_immediate_exn t
  | Value -> Array.unsafe_get (Obj_either.get_value_exn t) i

let unsafe_set t i x =
  match Obj_either.inspect t with
  | Immediate -> Obj_either.of_immediate x
  | Value ->
      Array.unsafe_set (Obj_either.get_value_exn t) i x;
      t

let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
  if len = 0 then dst
  else
    match Obj_either.inspect src with
    | Immediate ->
        let elt = Obj_either.get_immediate_exn src in
        assert (src_pos = 0);
        assert (len = 1);
        unsafe_set dst dst_pos elt
    | Value -> (
        let src = Obj_either.get_value_exn src in
        match Obj_either.inspect dst with
        | Immediate ->
            Format.eprintf "{ src_pos = %d; dst_pos = %d }@." src_pos dst_pos;
            assert (dst_pos = 0);
            assert (len = 1);
            Obj_either.of_immediate (Array.unsafe_get src src_pos)
        | Value ->
            let dst' = Obj_either.get_value_exn dst in
            Array.blit ~src ~src_pos ~dst:dst' ~dst_pos ~len;
            dst)

let singleton x = Obj_either.of_immediate x

let create ~len x =
  if len = 1 then Obj_either.of_immediate x
  else Obj_either.of_value (Array.make len x)

(** Here we use The Force to circumvent the value restriction. This is safe for
    the same reason that [Array.empty : 'a. 'a t] is safe: an empty array can
    never hold a value of the chosen type, so the type parameter is effectively
    phantom. *)
let empty = Obj.magic [||]
