let random_int () = Random.full_int Int.max_int

module Key = struct
  include Int

  let sexp_of_t = Base.Int.sexp_of_t
  let hash = Hashtbl.hash
  let hash_size = 30
end

let stabilize_garbage_collector () =
  let rec go fail last_heap_live_words =
    if fail <= 0 then
      failwith "Unable to stabilize the number of live words in the major heap";
    Gc.compact ();
    let stat = Gc.stat () in
    if stat.Gc.live_words <> last_heap_live_words then
      go (fail - 1) stat.Gc.live_words
  in
  go 10 0

let measure_size t = Obj.reachable_words (Obj.repr t)

let allocated_words () =
  let s = Gc.quick_stat () in
  s.minor_words +. s.major_words -. s.promoted_words

let run_loop ~name ~out ~iterations ~action t =
  let start_time = Mtime_clock.counter () in
  let last = ref Mtime.Span.zero in
  let initial_allocations = allocated_words () in
  stabilize_garbage_collector ();
  for i = 1 to iterations do
    action t;
    if i mod 1_000 = 0 then (
      let time = Mtime_clock.count start_time in
      let diff = Mtime.Span.abs_diff time !last in
      Printf.eprintf "\r%s : %#d / %#d%!" name i iterations;
      Printf.fprintf out "%d,%s,%d,%f,%Ld\n%!" i name (measure_size t)
        (allocated_words () -. initial_allocations)
        (Int64.div (Mtime.Span.to_uint64_ns diff) 1_000L);
      last := Mtime_clock.count start_time)
  done;
  Printf.eprintf "\r%s : done\n%!" name

let open_stat_file name =
  let stat_file =
    let rnd = Random.bits () land 0xFFFFFF in
    let ( / ) = Filename.concat in
    "_build" / Printf.sprintf "%s-%06x.csv" name rnd
  in
  Printf.printf "Sending stats to '%s'\n%!" stat_file;
  let out = open_out stat_file in
  Printf.fprintf out
    "entries,implementation,reachable_words,allocated_words,time(ns)\n";
  out

module Hashset = struct
  (** Compute metrics of various hashset implementations, as a function of the
      number of entries:

      - total size in memory
      - extra allocations per entry
      - cost of [add] per entry

      Stats are emitted to a trace file to be interpreted by [analysis/main.py]. *)

  module Hs_compact_imm = Compact.Hashset.Immediate
  module Hs_compact = Compact.Hashset.Immediate
  module Hs_backtracking = Hashset
  module Hs_base = Base.Hash_set
  module Hs_stdlib = Stdlib.Hashtbl

  module Hs_containers = struct
    include CCHashSet.Make (Key)

    let add t k = insert t k
  end

  let run_loop ~name ~out ~add t =
    run_loop t ~iterations:300_000 ~name ~out ~action:(fun t ->
        add t (random_int ()))

  let run () =
    let out = open_stat_file "hashset-memory-usage" in
    run_loop ~name:"stdlib" ~out
      ~add:(fun t k -> Hs_stdlib.add t k ())
      (Hs_stdlib.create 0);
    run_loop ~name:"backtracking" ~out ~add:Hs_backtracking.add
      (Hs_backtracking.create 0);
    run_loop ~name:"base" ~out ~add:Hs_base.add (Hs_base.create (module Key));
    run_loop ~name:"containers" ~out ~add:Hs_containers.add
      (Hs_containers.create 0);
    run_loop ~name:"compact-immediate" ~out ~add:Hs_compact_imm.add
      (Hs_compact_imm.create ~initial_capacity:0 (module Key));
    run_loop ~name:"compact" ~out ~add:Hs_compact.add
      (Hs_compact.create ~initial_capacity:0 (module Key));
    Printf.printf "\nDone\n"
end

module Hashtbl = struct
  module Hs_compact = Compact.Hashtbl
  module Hs_base = Base.Hashtbl
  module Hs_stdlib = Stdlib.Hashtbl

  let run_loop ~name ~out ~add t =
    run_loop t ~iterations:300_000 ~name ~out ~action:(fun t ->
        let k, v = (random_int (), random_int ()) in
        add t k v)

  let run () =
    let out = open_stat_file "hashtbl-memory-usage" in
    run_loop ~name:"compact" ~out ~add:Hs_compact.replace
      (Hs_compact.create ~initial_capacity:0 (module Key));
    run_loop ~name:"stdlib" ~out ~add:Hs_stdlib.add (Hs_stdlib.create 0);
    run_loop ~name:"base" ~out
      ~add:(fun t key data -> Hs_base.add_exn t ~key ~data)
      (Hs_base.create (module Key));
    Printf.printf "\nDone\n"
end

(* let () =
 *   Memtrace.trace_if_requested ();
 *   let total = 10_000_000 in
 *   let t = Compact.Hashset.Immediate.create ~initial_capacity:0 (module Key) in
 *   for _ = 1 to total do
 *     let n = random_int () in
 *     Compact.Hashset.Immediate.add t n
 *   done;
 *   Printf.eprintf "done\n%!" *)

let () = Hashtbl.run ()
