module I = Compact.Immediate_array

let get_size (t : _ I.t) = 1 + Obj.reachable_words (Obj.repr t)

let check_int pos ~expected actual =
  Alcotest.(check ~pos int) "" expected actual

let check_size pos ~expected t = check_int pos ~expected (get_size t)

let test_empty () =
  let t = I.empty in
  check_size __POS__ ~expected:1 t;
  I.invariant (fun _ -> assert false) t

let test_singleton () =
  let t = I.singleton () in
  check_size __POS__ ~expected:1 t;
  I.invariant ignore t

let test_length () =
  for i = 0 to 10 do
    let l = List.init i (fun _ -> ()) in
    let t = I.of_list l in
    let expected_size = match i with 0 -> 1 | 1 -> 1 | n -> 2 + n in
    check_int __POS__ ~expected:i (I.length t);
    check_size __POS__ ~expected:expected_size t;
    I.invariant ignore t
  done

let tests =
  let test name fn = Alcotest.test_case ("Immediate_array." ^ name) `Quick fn in
  [ test "empty" test_empty
  ; test "singleton" test_singleton
  ; test "length" test_length
  ]
