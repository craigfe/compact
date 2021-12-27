(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

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

(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
  ————————————————————————————————————————————————————————————————————————————*)
