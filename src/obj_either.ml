(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

type ('a, 'b) t = Obj.t

let of_immediate : type a b. a -> (a, b) t =
 fun a ->
  if Obj.is_block (Obj.repr a) then
    failwith "Obj_either.of_immediate: passed a heap-allocated value";
  Obj.repr a

let of_value : type a b. b -> (a, b) t =
 fun b ->
  if Obj.is_int (Obj.repr b) then
    failwith "Obj_either.of_value: passed an immediate value";
  Obj.repr b

type state = Immediate | Value

let inspect t = if Obj.is_int t then Immediate else Value

let get_immediate_exn : type a b. (a, b) t -> a =
 fun t ->
  match inspect t with
  | Immediate -> Obj.obj t
  | Value ->
      failwith "Obj_either.get_immediate_exn: passed a heap-allocated value"

let get_value_exn : type a b. (a, b) t -> b =
 fun t ->
  match inspect t with
  | Value -> Obj.obj t
  | Immediate -> failwith "Obj_either.get_value_exn: passed an immediate value"

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
