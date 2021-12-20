(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

module type S0 = sig
  type t
  type outer_t

  val repr : (outer_t, t) Type_equality.t
end

module type S1 = sig
  type 'a t
  type 'a outer_t

  val repr : ('a outer_t, 'a t) Type_equality.t
end

module type S1_plus = sig
  type +'a t
  type +'a outer_t

  val repr : ('a outer_t, 'a t) Type_equality.t
end

module type S2 = sig
  type ('a, 'b) t
  type ('a, 'b) outer_t

  val repr : (('a, 'b) outer_t, ('a, 'b) t) Type_equality.t
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t
  type ('a, 'b, 'c, 'd, 'e) outer_t

  val repr :
    (('a, 'b, 'c, 'd, 'e) outer_t, ('a, 'b, 'c, 'd, 'e) t) Type_equality.t
end

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
