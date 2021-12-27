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

module String_set = struct
  module String_set = Hashset.Fixed_size_string

  let test_simple () =
    let set = String_set.create ~elt_length:1 ~initial_capacity:0 in
    String_set.mem set "a" |> check_bool __POS__ ~expected:false;
    String_set.add set "a";
    String_set.mem set "a" |> check_bool __POS__ ~expected:true;
    String_set.add set "b";
    String_set.add set "c";
    String_set.mem set "a" |> check_bool __POS__ ~expected:true;
    String_set.mem set "b" |> check_bool __POS__ ~expected:true;
    String_set.mem set "c" |> check_bool __POS__ ~expected:true

  let test_random () =
    let elt_length = 32 in
    let set = String_set.create ~elt_length ~initial_capacity:0 in
    let reference_tbl = Stdlib.Hashtbl.create 0 in
    let reference_vector = Vector.create ~dummy:"" in
    let random_string () =
      String.init elt_length (fun _ -> char_of_int (Random.int 256))
    in
    for i = 0 to 1_000_000 do
      (* Add a new element. *)
      let new_elt = random_string () in
      String_set.add set new_elt;
      Stdlib.Hashtbl.add reference_tbl new_elt ();
      Vector.push reference_vector new_elt;

      (* Pick a random existing element and check [mem] is true. *)
      let elt = Vector.get reference_vector (Random.int (i + 1)) in
      assert (Stdlib.Hashtbl.mem reference_tbl elt);
      String_set.mem set elt |> check_bool __POS__ ~expected:true;

      (* Pick a random non-existing element and check [mem] is false. *)
      let non_elt = random_string () in
      assert (not (Stdlib.Hashtbl.mem reference_tbl non_elt));
      String_set.mem set non_elt |> check_bool __POS__ ~expected:false
    done
end

let () =
  let test name fn = Alcotest.test_case name `Quick fn in
  Alcotest.run __FILE__
    [ ( "main"
      , [ test "String_set.simple" String_set.test_simple
        ; test "String_set.random" String_set.test_random
        ] )
    ; ("Arena", Test_arena.tests)
    ; ("Hashed_container", Test_hashed_container.tests)
    ; ("Hashtbl", Test_hashtbl.tests)
    ; ("Immediate_array", Test_immediate_array.tests)
    ]
