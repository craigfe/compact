open! Compact

module Int_key = struct
  include Stdlib.Int

  let hash = Stdlib.Hashtbl.hash
  let hash_size = 30
end

module String_key = struct
  include Stdlib.String

  let hash = Stdlib.Hashtbl.hash
  let hash_size = 30
end

let expect_failure ?(msg = "") pos ~f =
  match f () with
  | exception Failure _ -> ()
  | exception _ -> assert false
  | _ -> Alcotest.fail ~pos msg

let test_non_immediate_entry () =
  let module T = Hashset.Immediate in
  let t = T.create (module String_key) ~initial_capacity:0 in
  expect_failure __POS__ ~f:(fun () -> T.add t "strings are not immediates")

let test_non_immediate64_entry () =
  let module T = Hashset.Immediate64 in
  let t = T.create (module String_key) ~initial_capacity:0 in
  let f () = T.add t "strings are not immediates" in
  match Sys.word_size with
  | 64 -> expect_failure __POS__ ~f
  | _ ->
      (* Adding the string should work, since [Immediate64] isn't attempting to
         inline singleton arrays: *)
      f ()

let test_mutation_while_iterating () =
  let t = Hashset.create (module Int_key) ~initial_capacity:1 in
  Hashset.add t 1;

  (* Attempting a mutation during an iteration is forbidden: even if the
     mutation has no effect (e.g. we add an element that already exists): *)
  let expect_failure =
    expect_failure ~msg:"Expected mutation while iterating to raise Failure"
  in
  let expect_fail_in_iter pos ~f =
    Hashset.iter t ~f:(function
      | 1 -> expect_failure pos ~f
      | _ -> assert false)
  in
  expect_fail_in_iter __POS__ ~f:(fun () -> Hashset.add t 1);
  expect_fail_in_iter __POS__ ~f:(fun () -> Hashset.add t 2);
  expect_fail_in_iter __POS__ ~f:(fun () -> Hashset.remove t 1);
  expect_fail_in_iter __POS__ ~f:(fun () -> Hashset.clear t);

  (* Iteration should be re-entrant, and mutation in the outer loop should still
     be forbidden: *)
  Hashset.iter t ~f:(fun _ ->
      Hashset.iter t ~f:(fun _ -> ());
      expect_failure __POS__ ~f:(fun () -> Hashset.add t 2));

  (* Mutation should be allowed after finishing an iteration, even if we abort with an exception: *)
  let () =
    let exception Exit in
    match Hashset.iter t ~f:(fun _ -> raise Exit) with
    | () -> assert false
    | exception Exit -> Hashset.add t 1
  in

  ()

let test_bucket_count () =
  let module Key = struct
    type t = int

    let equal = ( = )
    let compare = Stdlib.compare
    let hash x = x
    let hash_size = 30
  end in
  let t = Hashset.create (module Key) ~initial_capacity:0 in
  let check_buckets pos expected =
    Alcotest.(check ~pos int) "" expected (Hashset.bucket_count t)
  in
  let check_load_factor pos expected =
    Alcotest.(check ~pos (float 0.)) "" expected (Hashset.load_factor t)
  in
  let add_elt =
    let count = ref (-1) in
    fun () ->
      incr count;
      Hashset.add t !count
  in
  let initial_buckets = 16 in
  let max_load_factor = 2 in

  (* Initially the set is empty: *)
  check_buckets __POS__ initial_buckets;
  check_load_factor __POS__ 0.;

  (* We fill it up to its initial capacity: *)
  for _ = 1 to max_load_factor * initial_buckets do
    add_elt ()
  done;
  check_buckets __POS__ initial_buckets;
  check_load_factor __POS__ (Float.of_int max_load_factor);

  (* We exceed the max load factor, triggering a resize: *)
  add_elt ();
  check_buckets __POS__ (2 * initial_buckets);
  check_load_factor __POS__ (1. +. (1. /. Float.of_int (2 * initial_buckets)));

  (* Clearing sets the number of buckets to 1: *)
  Hashset.clear t;
  check_buckets __POS__ 1;
  check_load_factor __POS__ 0.;
  ()

let tests =
  let test name fn = Alcotest.test_case ("Hashset." ^ name) `Quick fn in
  [ test "Immediate.non_immediate_entry" test_non_immediate_entry
  ; test "Immediate.non_immediate64_entry" test_non_immediate64_entry
  ; test "mutation_while_iterating" test_mutation_while_iterating
  ; test "bucket_count" test_bucket_count
  ]
