open! Compact

let check_bool pos ~expected actual =
  Alcotest.(check ~pos bool) "" expected actual

let check_invalid_arg pos f =
  let fail got =
    Alcotest.failf ~pos
      "Expected function to raise `Invalid_argument`, but raised: %a"
      Fmt.(Dump.option exn)
      got
  in
  match f () with
  | _ -> fail None
  | exception Invalid_argument _ -> ()
  | exception exn -> fail (Some exn)

let test_is_full () =
  let arena = Arena.create ~elt_length:1 ~initial_capacity:0 in
  Arena.is_full arena |> check_bool __POS__ ~expected:true;
  Arena.expand arena 1;
  Arena.is_full arena |> check_bool __POS__ ~expected:false;
  let (_ : Arena.id) = Arena.allocate arena "x" in
  Arena.is_full arena |> check_bool __POS__ ~expected:true

(* Exercises [allocate] and [dereference]. *)
let test_read_write () =
  let arena = Arena.create ~elt_length:1 ~initial_capacity:0 in
  check_invalid_arg __POS__ (fun () -> Arena.allocate arena "x");

  (* Add some elements and ensure they're dereferenced correctly: *)
  Arena.expand arena 4;
  let elts = [ "a"; "b"; "c"; "d" ] in
  let ids = List.map (Arena.allocate arena) elts in
  check_invalid_arg __POS__ (fun () -> Arena.allocate arena "e");
  Stdlib.ListLabels.iter2 elts ids ~f:(fun expected id ->
      let got = Arena.dereference arena id in
      Alcotest.(check string) "Element is dereferenced correctly" expected got)

let test_expand () =
  let arena = Arena.create ~elt_length:100 ~initial_capacity:0 in
  Arena.expand arena 0 (* No-op expands are fine *);
  check_invalid_arg __POS__ (fun () -> Arena.expand arena (-1));
  Arena.expand arena 1;
  Arena.expand arena 3;

  (* Not allowed to contract the arena (even when the space is unused): *)
  check_invalid_arg __POS__ (fun () -> Arena.expand arena 2)

let test_elt_equal () =
  let arena = Arena.create ~elt_length:1 ~initial_capacity:1 in
  let a_ref = Arena.allocate arena "a" in
  Arena.elt_equal arena a_ref "a" |> check_bool __POS__ ~expected:true;
  Arena.elt_equal arena a_ref "b" |> check_bool __POS__ ~expected:false;
  ()

let test_smuggled_id () =
  let elt_length = 30 in
  let arena = Arena.create ~elt_length ~initial_capacity:3 in

  (* Build an invalid ID into [arena] by interacting with a different one: *)
  let smuggled_id =
    let elt_length = 50 in
    let arena = Arena.create ~elt_length ~initial_capacity:2 in
    let elt = String.make elt_length 'x' in
    (* Allocate a string of length 50 at offset 0, then another string
       immediately after it, returning a pointer to offset 50. *)
    let (_ : Arena.id) = Arena.allocate arena elt in
    Arena.allocate arena elt
  in
  let check_deref_invalid pos =
    check_invalid_arg pos (fun () -> Arena.dereference arena smuggled_id);
    check_invalid_arg pos (fun () -> Arena.elt_equal arena smuggled_id "")
  in

  check_deref_invalid __POS__ (* id = 50, Arena offset = 0 *);
  let (_ : Arena.id) = Arena.allocate arena (String.make elt_length 'a') in
  check_deref_invalid __POS__ (* id = 50, Arena offset = 30 *);
  let (_ : Arena.id) = Arena.allocate arena (String.make elt_length 'b') in
  check_deref_invalid __POS__ (* id = 50, arena offset = 60 (elt len = 30) *);
  let (_ : Arena.id) = Arena.allocate arena (String.make elt_length 'c') in

  (* This time, the smuggled ID is a 'valid' pointer into the new arena, so we
     can't guard against invalid usage. We read over the boundary between
     elements 2 and 3 instead: *)
  let result = Arena.dereference arena smuggled_id in
  let expected = String.make 10 'b' ^ String.make 20 'c' in
  Alcotest.(check ~pos:__POS__ string) "" expected result

let test_invalid_length () =
  let arena = Arena.create ~elt_length:1 ~initial_capacity:100 in
  check_invalid_arg __POS__ (fun () -> Arena.allocate arena "")

let tests =
  let test name fn = Alcotest.test_case ("Arena." ^ name) `Quick fn in
  [ test "is_full" test_is_full
  ; test "read_write" test_read_write
  ; test "expand" test_expand
  ; test "elt_equal" test_elt_equal
  ; test "smuggled_id" test_smuggled_id
  ; test "invalid_length" test_invalid_length
  ]
