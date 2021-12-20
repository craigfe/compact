(** This module provides a very generic hashtable implementation that allows the
    internal memory layout to be customised to particular types. It allows the
    user to specify:

    - the type of keys and values in the hashtable;
    - the type of {i bindings}: internal representations of key / value pairs
      used in the hashtable itself (and the number of words assigned to each
      binding).
    - the number of in-memory words used to store the keys and values (may be 1,
      2 or 3);
    - the representation of internal bindings.

    Together, these allow this implementation to be used in various compact
    deployments:

    - as a hash-set (with no redundant internal space for dummy values, as in a
      [unit Stdlib.Hashtbl.t]); *)

include Hashed_container_intf.Intf
