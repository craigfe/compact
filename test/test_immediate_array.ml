module I = Compact.Immediate_array

let get_size (t : _ I.t) = 1 + Obj.reachable_words (Obj.repr t)

let check_int pos ~expected actual =
  Alcotest.(check ~pos int) "" expected actual

let test_empty () =
  let t = I.empty in
  I.invariant (fun _ -> assert false) t

let test_singleton () =
  let t = I.singleton () in
  I.invariant ignore t

let test_length () =
  let check_size pos ~expected t =
    List.mem (get_size t) expected
    |> Alcotest.(check ~pos bool)
         (Fmt.str "Expected one of %a" Fmt.(Dump.list int) expected)
         true
  in
  let expected_size_of_length = function
    | 0 ->
        (* NOTE: when using [--disable-naked-pointers] (or the multicore OCaml
           runtime), atoms like the empty array are included in [Obj.reachable_words],
           making the "size" of an empty immediate array is [2] rather than [1]. *)
        [ 1; 2 ]
    | 1 -> [ 1 ]
    | n -> [ 2 + n ]
  in
  for i = 0 to 10 do
    let l = List.init i (fun _ -> ()) in
    let t = I.of_list l in
    check_int __POS__ ~expected:i (I.length t);
    check_size __POS__ ~expected:(expected_size_of_length i) t;
    I.invariant ignore t
  done

let tests =
  let test name fn = Alcotest.test_case ("Immediate_array." ^ name) `Quick fn in
  [ test "empty" test_empty
  ; test "singleton" test_singleton
  ; test "length" test_length
  ]
