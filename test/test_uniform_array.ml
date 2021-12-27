open! Compact_test_helpers.Import
open! Compact
open Uniform_array

let zero_obj = Obj.repr (0 : int)
let phys_equal = ( == )
let does_raise f = match ignore (f ()) with () -> false | exception _ -> true

let test_create_obj_array () =
  let t = create_obj_array ~len:0 in
  assert (length t = 0)

let test_create () =
  let str = Obj.repr "foo" in
  let t = create ~len:2 str in
  assert (phys_equal (get t 0) str);
  assert (phys_equal (get t 1) str)

let test_float_elements () =
  let float = Obj.repr 3.5 in
  let t = create ~len:2 float in
  assert (Obj.tag (Obj.repr t) = 0);
  (* not a double array *)
  assert (phys_equal (get t 0) float);
  assert (phys_equal (get t 1) float);
  set t 1 (Obj.repr 4.);
  assert (Float.equal (Obj.obj (get t 1)) 4.)

let test_empty () =
  assert (length empty = 0);
  assert (does_raise (fun () -> get empty 0))

let test_singleton () =
  assert (length (singleton zero_obj) = 1);
  assert (phys_equal (get (singleton zero_obj) 0) zero_obj);
  assert (does_raise (fun () -> get (singleton zero_obj) 1));

  let f = 13. in
  let t = singleton (Obj.repr f) in
  invariant ignore t;
  assert (Obj.repr f = get t 0)

(* [get], [unsafe_get], [set], [unsafe_set], [unsafe_set_assuming_currently_int] *)
let test_get_and_set () =
  let t = create_obj_array ~len:1 in
  assert (length t = 1);
  assert (phys_equal (get t 0) zero_obj);
  assert (phys_equal (unsafe_get t 0) zero_obj);
  let one_obj = Obj.repr (1 : int) in
  let check_get expect =
    assert (phys_equal (get t 0) expect);
    assert (phys_equal (unsafe_get t 0) expect)
  in
  set t 0 one_obj;
  check_get one_obj;
  unsafe_set t 0 zero_obj;
  check_get zero_obj;
  unsafe_set_assuming_currently_int t 0 one_obj;
  check_get one_obj

let test_exists () =
  let test arr f = of_list arr |> exists ~f in
  let r pos expected actual = Alcotest.(check ~pos bool) "" expected actual in
  r __POS__ false (test [] Fun.id);
  r __POS__ true (test [ true ] Fun.id);
  r __POS__ true (test [ false; false; false; false; true ] Fun.id);
  r __POS__ true (test [ 0; 1; 2; 3; 4 ] (fun i -> i mod 2 = 1));
  r __POS__ false (test [ 0; 2; 4; 6; 8 ] (fun i -> i mod 2 = 1))

let test_iteri () =
  let test pos ~expected arr =
    let acc = ref [] in
    of_list arr |> iteri ~f:(fun i x -> acc := (i, x) :: !acc);
    Alcotest.(check ~pos (list (pair int char))) "" expected (List.rev !acc)
  in
  test __POS__ ~expected:[] [];
  test __POS__ ~expected:[ (0, 'a') ] [ 'a' ];
  test __POS__
    ~expected:[ (0, 'a'); (1, 'b'); (2, 'c'); (3, 'd') ]
    [ 'a'; 'b'; 'c'; 'd' ]

let tests =
  let test name fn = Alcotest.test_case ("Uniform_array." ^ name) `Quick fn in
  [ test "create_obj_array" test_create_obj_array
  ; test "create" test_create
  ; test "float_elements" test_float_elements
  ; test "empty" test_empty
  ; test "singleton" test_singleton
  ; test "get_and_set" test_get_and_set
  ; test "exists" test_exists
  ; test "iteri" test_iteri
  ]
