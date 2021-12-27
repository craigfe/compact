<h1 align="center">Compact</h1>
<h4 align="center">Memory-efficient data structures for OCaml</h4>
<br>

Currently provides the following modules:

- **`Arena`**: an arena for fixed-width strings;

- **`Uniform_array`**: an array implementation that forbids the [flat float
  array optimisation][flat-float-array]. Comes with `Tuple2` and `Tuple3`
  specialisations for compact arrays of _pairs_ and _triples_.

- **`Hashset`**: an unordered set container. Comes with three specialisations:
  - `Immediate`, for elements with an immediate representation
    (e.g. `int`).
  - `Immediate64`, for elements with an immediate 
    representation on 64-bit platforms only (e.g. `Int63.t`).
  - `Fixed_size_string`, for elements that are strings of a fixed length.

- **`Hashtbl`**: an unordered associative container.

- **`Hashed_container`**: a generic hashtable implementation with support for
  externally-allocated bindings.

See the [`./bench` subdirectory](./bench/) for benchmarks (and benchmark
results) for the above modules.

[flat-float-array]: https://dev.realworldocaml.org/runtime-memory-layout.html#scrollNav-3-1

### Installation

`Compact` can be installed with `opam`:

```
opam pin add -n compact.dev git+https://github.com/CraigFe/compact
opam install compact
```

### Acknowledgements

This library pulls code and ideas from various open-source Jane Street
libraries, including a [uniform array][base-uniform-array] implementation from
`Base`.

[base-uniform-array]: https://github.com/janestreet/base/blob/caae3c2bacebd558ae2b0bbea807ec9703fb7508/src/uniform_array.ml
