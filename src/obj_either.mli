(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(** A variant of the [Either.t] type that distinguishes [Left] and [Right] cases
    by their {i immediacy} (ie. whether [Obj.is_int] holds of the contents)
    rather than a heap-allocated variant. Using this type with types that are
    conditionally-immediate (such as [Int63.t]) will result in undefined
    behaviour.

    This module is not exposed for external use. *)

type ('a, 'b) t = private Obj.t

val of_immediate : 'a -> ('a, _) t
val of_value : 'b -> (_, 'b) t

type state = Immediate | Value

val inspect : (_, _) t -> state
val get_immediate_exn : ('a, _) t -> 'a
val get_value_exn : (_, 'b) t -> 'b

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
