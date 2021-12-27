open! Compact_test_helpers.Import
open! Compact

module String_key = struct
  include Stdlib.String

  let hash = Stdlib.Hashtbl.hash
  let hash_size = 30
end

let test_data = [ ("a", 1); ("b", 2); ("c", 3) ]

let test_hash =
  let h = Hashtbl.create ~initial_capacity:10 (module String_key) in
  List.iter test_data ~f:(fun (k, v) -> Hashtbl.replace h ~key:k ~data:v);
  h

let test_find () =
  let found = Hashtbl.find test_hash "a" in
  let not_found = Hashtbl.find test_hash "A" in
  Hashtbl.invariant ignore ignore test_hash;
  assert (match (found, not_found) with Some _, None -> true | _ -> false)

let test_findi_and_call () =
  let our_hash = Hashtbl.copy test_hash in
  let test_string = "test string" in
  Hashtbl.add_exn our_hash ~key:test_string ~data:10;
  let test_string' = "test " ^ "string" in
  assert (not (test_string == test_string'));
  assert (
    Hashtbl.find_and_call our_hash test_string'
      ~if_found:(fun ~key ~data -> test_string == key && data = 10)
      ~if_not_found:(fun _ -> false))

let test_add () =
  let our_hash = Hashtbl.copy test_hash in
  let duplicate = Hashtbl.add our_hash ~key:"a" ~data:4 in
  let no_duplicate = Hashtbl.add our_hash ~key:"d" ~data:5 in
  assert (Hashtbl.find our_hash "a" = Some 1);
  assert (Hashtbl.find our_hash "d" = Some 5);
  Hashtbl.invariant ignore ignore our_hash;
  assert (
    match (duplicate, no_duplicate) with `Duplicate, `Ok -> true | _ -> false)

let test_iter () =
  let cmp x y = -Int.compare x y in
  let predicted = List.sort ~cmp (List.map test_data ~f:(fun (_, v) -> v)) in
  let found =
    let found = ref [] in
    Hashtbl.iter test_hash ~f:(fun ~key:_ ~data:v -> found := v :: !found);
    !found |> List.sort ~cmp
  in
  assert (List.equal ~eq:Int.equal predicted found)

let test_iter_keys () =
  let cmp x y = -String.compare x y in
  let predicted = List.sort ~cmp (List.map test_data ~f:(fun (k, _) -> k)) in
  let found =
    let found = ref [] in
    Hashtbl.iter_keys test_hash ~f:(fun k -> found := k :: !found);
    !found |> List.sort ~cmp
  in
  assert (List.equal ~eq:String.equal predicted found)

(* let test_of_alist_size () =
 *   let predicted = List.length test_data in
 *   let found = Hashtbl.cardinal (Hashtbl.of_alist_poly_exn test_data) in
 *   predicted = found
 * 
 * let test_of_alist_right_keys () =
 *   let predicted = List.map test_data ~f:(fun (k, _) -> k) in
 *   let found = Hashtbl.keys (Hashtbl.of_alist_poly_exn test_data) in
 *   let sp = List.sort ~cmp:Stdlib.compare predicted in
 *   let sf = List.sort ~cmp:Stdlib.compre found in
 *   sp = sf *)

(* let%test_module "of_alist_or_error" =
 *   (module struct
 *     let test_unique = Result.is_ok (Hashtbl.of_alist_poly_or_error test_data)
 * 
 *     let test_duplicate =
 *       Result.is_error (Hashtbl.of_alist_poly_or_error (test_data @ test_data))
 *   end) *)

let test_size_and_right_keys () =
  let predicted = List.map test_data ~f:(fun (k, _) -> k) in
  let found = Hashtbl.to_list test_hash |> List.map ~f:fst in
  let sp = List.sort ~cmp:Stdlib.compare predicted in
  let sf = List.sort ~cmp:Stdlib.compare found in
  assert (sp = sf)

let test_size_and_right_data () =
  let predicted = List.map test_data ~f:(fun (_, v) -> v) in
  let found = Hashtbl.to_list test_hash |> List.map ~f:snd in
  let sp = List.sort ~cmp:Stdlib.compare predicted in
  let sf = List.sort ~cmp:Stdlib.compare found in
  assert (sp = sf)

let test_map () =
  let add1 x = x + 1 in
  let predicted_data =
    List.sort ~cmp:Stdlib.compare
      (List.map test_data ~f:(fun (k, v) -> (k, add1 v)))
  in
  let found_alist =
    Hashtbl.map test_hash ~f:add1
    |> Hashtbl.to_list
    |> List.sort ~cmp:Stdlib.compare
  in
  assert (List.equal ~eq:Stdlib.( = ) predicted_data found_alist)

let test_map_inplace () =
  let f x = x + 3 in
  let predicted_data =
    List.sort ~cmp:Stdlib.compare
      (List.map test_data ~f:(fun (k, v) -> (k, f v)))
  in
  let test_hash = Hashtbl.copy test_hash in
  Hashtbl.map_inplace test_hash ~f;
  let found_alist =
    Hashtbl.to_list test_hash |> List.sort ~cmp:Stdlib.compare
  in
  assert (List.equal ~eq:Stdlib.( = ) predicted_data found_alist)

let test_insert_find_remove () =
  let t = Hashtbl.create_poly () ~initial_capacity:1 in
  let inserted = ref [] in
  Random.init 123;
  let verify_inserted t =
    let missing =
      List.fold_left !inserted ~init:[] ~f:(fun acc (key, data) ->
          match Hashtbl.find t key with
          | None -> `Missing key :: acc
          | Some d -> if data = d then acc else `Wrong_data (key, data) :: acc)
    in
    match missing with [] -> () | _ -> failwith "some inserts are missing"
  in
  let rec loop i t =
    if i < 2000 then (
      let k = Random.int 10_000 in
      inserted := (k, i) :: List.remove_assoc k !inserted;
      Hashtbl.replace t ~key:k ~data:i;
      Hashtbl.invariant ignore ignore t;
      verify_inserted t;
      loop (i + 1) t)
  in
  loop 0 t;
  List.iter !inserted ~f:(fun (x, _) ->
      Hashtbl.remove t x;
      Hashtbl.invariant ignore ignore t;
      (match Hashtbl.find t x with
      | None -> ()
      | Some _ -> Printf.ksprintf failwith "present after removal: %d" x);
      inserted := List.remove_assoc x !inserted;
      verify_inserted t)

let list_take t_orig n =
  if n <= 0 then []
  else
    let rec loop n t accum =
      if n = 0 then List.rev accum
      else
        match t with [] -> t_orig | hd :: tl -> loop (n - 1) tl (hd :: accum)
    in
    loop n t_orig []

let test_clear () =
  let t = Hashtbl.create_poly () ~initial_capacity:1 in
  let l = List.init ~len:100 ~f:Fun.id in
  let verify_present l = List.for_all l ~f:(Hashtbl.mem t) in
  let verify_not_present l =
    List.for_all l ~f:(fun i -> not (Hashtbl.mem t i))
  in
  List.iter l ~f:(fun i -> Hashtbl.replace t ~key:i ~data:(i * i));
  List.iter l ~f:(fun i -> Hashtbl.replace t ~key:i ~data:(i * i));
  assert (Hashtbl.cardinal t = 100);
  assert (verify_present l);
  Hashtbl.clear t;
  Hashtbl.invariant ignore ignore t;
  assert (Hashtbl.cardinal t = 0);
  assert (verify_not_present l);
  let l = list_take l 42 in
  List.iter l ~f:(fun i -> Hashtbl.replace t ~key:i ~data:(i * i));
  assert (Hashtbl.cardinal t = 42);
  assert (verify_present l);
  Hashtbl.invariant ignore ignore t

let test_mem () =
  let t = Hashtbl.create_poly () ~initial_capacity:1 in
  Hashtbl.invariant ignore ignore t;
  assert (not (Hashtbl.mem t "Fred"));
  Hashtbl.invariant ignore ignore t;
  Hashtbl.replace t ~key:"Fred" ~data:"Wilma";
  Hashtbl.invariant ignore ignore t;
  assert (Hashtbl.mem t "Fred");
  Hashtbl.invariant ignore ignore t;
  Hashtbl.remove t "Fred";
  Hashtbl.invariant ignore ignore t;
  assert (not (Hashtbl.mem t "Fred"));
  Hashtbl.invariant ignore ignore t

let test_exists () =
  let t = Hashtbl.create_poly ~initial_capacity:0 () in
  assert (not (Hashtbl.exists t ~f:(fun ~key:_ -> failwith "can't be called")));
  Hashtbl.replace t ~key:7 ~data:3;
  assert (not (Hashtbl.exists t ~f:(fun ~key:_ ~data -> Int.equal 4 data)));
  Hashtbl.replace t ~key:8 ~data:4;
  assert (Hashtbl.exists t ~f:(fun ~key:_ ~data -> Int.equal 4 data))
(* Hashtbl.replace t ~key:9 ~data:5;
 * assert (Hashtbl.existsi t ~f:(fun ~key ~data -> key + data = 14)) *)

let test_for_all () =
  let t = Hashtbl.create_poly ~initial_capacity:0 () in
  assert (Hashtbl.for_all t ~f:(fun ~key:_ -> failwith "can't be called"));
  Hashtbl.replace t ~key:7 ~data:3;
  assert (Hashtbl.for_all t ~f:(fun ~key:_ ~data:x -> Int.equal x 3));
  Hashtbl.replace t ~key:8 ~data:4;
  assert (not (Hashtbl.for_all t ~f:(fun ~key:_ ~data:x -> Int.equal x 3)))
(* Hashtbl.replace t ~key:9 ~data:5;
 * assert (Hashtbl.for_alli t ~f:(fun ~key ~data -> key - 4 = data)) *)

let test_count () =
  let t = Hashtbl.create_poly ~initial_capacity:0 () in
  assert (Hashtbl.count t ~f:(fun ~key:_ -> failwith "can't be called") = 0);
  Hashtbl.replace t ~key:7 ~data:3;
  assert (Hashtbl.count t ~f:(fun ~key:_ ~data:x -> Int.equal x 3) = 1);
  Hashtbl.replace t ~key:8 ~data:4;
  assert (Hashtbl.count t ~f:(fun ~key:_ ~data:x -> Int.equal x 3) = 1)
(* Hashtbl.replace t ~key:9 ~data:5;
 * assert (Hashtbl.counti t ~f:(fun ~key ~data -> key - 4 = data) = 3) *)

let tests =
  let test name fn = Alcotest.test_case ("Hashtbl." ^ name) `Quick fn in
  [ test "find" test_find
  ; test "findi_and_call" test_findi_and_call
  ; test "add" test_add
  ; test "iter" test_iter
  ; test "iter_keys" test_iter_keys
  ; test "size_and_right_keys" test_size_and_right_keys
  ; test "size_and_right_data" test_size_and_right_data
  ; test "map" test_map
  ; test "map_inplace" test_map_inplace
  ; test "insert_find_remove" test_insert_find_remove
  ; test "clear" test_clear
  ; test "mem" test_mem
  ; test "exists" test_exists
  ; test "for_all" test_for_all
  ; test "count" test_count
  ]
