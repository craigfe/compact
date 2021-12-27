(lang dune 2.9)
(name compact)
(implicit_transitive_deps false)

(generate_opam_files true)
(source (github CraigFe/compact))
(license MIT)
(maintainers "Craig Ferguson <me@craigfe.io>")
(authors "Craig Ferguson <me@craigfe.io>")

(package
 (name compact)
 (synopsis "Containers with small memory footprints")
 (description "Containers with small memory footprints")
 (documentation https://CraigFe.github.io/compact/)
 (depends
  (ocaml (>= 4.12.0))
  bigstringaf
  (bisect_ppx :dev)
  (vector :with-test)
  (fmt :with-test)
  (alcotest (and :with-test (>= 1.4.0)))))

(package
 (name compact-bench)
 (synopsis "Benchmarks for the `compact` library")
 (description "Benchmarks for the `compact` library")
 (depends
  (ocaml (>= 4.12.0))
  (compact (= :version))
  base
  hashset
  (mtime (>= 1.3.0))
  memtrace
  sexplib0))