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
  Printf.eprintf "\r%s : done\x1b[K\n%!" name

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

  module type S = sig
    type t

    val name : string
    val create : int -> t
    val add : t -> int -> unit
  end

  module Hs_compact_imm : S = struct
    module T = Compact.Hashset.Immediate

    type t = int T.t

    let name = "compact-immediate"
    let create n = T.create ~initial_capacity:n (module Key)
    let add = T.add
  end

  module Hs_compact : S = struct
    module T = Compact.Hashset

    type t = int T.t

    let name = "compact"
    let create n = T.create ~initial_capacity:n (module Key)
    let add = T.add
  end

  module Hs_backtracking : S = struct
    module T = Hashset

    type nonrec t = int Hashset.t

    let name = "backtracking"
    let create = T.create
    let add = T.add
  end

  module Hs_base : S = struct
    module T = Base.Hash_set

    type t = int T.t

    let name = "base"
    let create n = T.create ~size:n (module Key)
    let add = T.add
  end

  module Hs_stdlib : S = struct
    module T = Stdlib.Hashtbl

    type nonrec t = (int, unit) T.t

    let name = "stdlib"
    let create n = T.create n
    let add t k = T.add t k ()
  end

  module Hs_containers : S = struct
    let name = "containers"

    include CCHashSet.Make (Key)

    let add t k = insert t k
  end

  let run_loop ~out (module Hashset : S) =
    let t = Hashset.create 0 in
    run_loop t ~iterations:300_000 ~name:Hashset.name ~out ~action:(fun t ->
        Hashset.add t (random_int ()))

  let run () =
    let out = open_stat_file "hashset-memory-usage" in
    List.iter (run_loop ~out)
      [ (module Hs_stdlib)
      ; (module Hs_backtracking)
      ; (module Hs_base)
      ; (module Hs_containers)
      ; (module Hs_compact_imm)
      ; (module Hs_compact)
      ];
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
    run_loop ~name:"compact" ~out
      ~add:(fun t key data -> Hs_compact.replace t ~key ~data)
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

let () =
  match Sys.argv with
  | [| _; "hashtbl" |] -> Hashtbl.run ()
  | [| _; "hashset" |] -> Hashset.run ()
  | _ ->
      Printf.eprintf "usage: %s [hashtbl | hashset]\n%!" Sys.argv.(0);
      exit 1
