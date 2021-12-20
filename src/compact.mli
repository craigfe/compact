(** Containers with compact memory footprints. *)

(** {2 Generic containers} *)

module Hashtbl = Hashtbl
module Hashset = Hashset
module Hashed_container = Hashed_container
module Uniform_array = Uniform_array

(** {2 Containers for fixed-length strings} *)

module Arena = Arena
(** An implementation of append-only arenas of fixed-length strings, with
    support for manual expansion. *)

(** Utility modules: *)

module Type_equality = Type_equality
module Internal = Internal
