type 'a t = 'a -> unit
type 'a inv = 'a t

module type S = sig
  type t

  val invariant : t inv
end

module type S1 = sig
  type 'a t

  val invariant : 'a inv -> 'a t inv
end

module type S2 = sig
  type ('a, 'b) t

  val invariant : 'a inv -> 'b inv -> ('a, 'b) t inv
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val invariant : 'a inv -> 'b inv -> 'c inv -> ('a, 'b, 'c) t inv
end

module type Intf = sig
  module type S = S
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3
end
