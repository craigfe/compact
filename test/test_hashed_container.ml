open! Compact

module Key = struct
  include Stdlib.Int

  let hash = Stdlib.Hashtbl.hash
  let hash_size = 30
end

let test_mutation_while_iterating () =
  let t = Hashset.create (module Key) ~initial_capacity:1 in
  Hashset.add t 1;

  let expect_failure pos ~f =
    match f () with
    | exception Failure _ -> ()
    | exception _ -> assert false
    | _ ->
        (* | (exception _) -> *)
        Alcotest.fail ~pos "Expected mutation while iterating to raise Failure"
  in

  (* Attempting a mutation during an iteration is forbidden: even if the
     mutation has no effect (e.g. we add an element that already exists): *)
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

let tests =
  let test name fn = Alcotest.test_case name `Quick fn in
  [ test "Hashset.mutation_while_iterating" test_mutation_while_iterating ]
