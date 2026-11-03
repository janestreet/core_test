open Core

(* We just want to make sure all of this compiles. *)

module _ = struct
  type t = Set.M(Int).t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp]
end

module _ : sig
  type t = Int.Set.t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, stable_witness]
end = struct
  type t = Set.Stable.V1.M(Int.Stable.V1).t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, stable_witness]
end

module _ = struct
  type t0 = float Map.M(Int).t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp]

  type 'a t1 = 'a Map.M(Bool).t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp]
end

module _ : sig
  type t0 = float Int.Map.t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, stable_witness]

  type 'a t1 = 'a Bool.Map.t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, stable_witness]
end = struct
  type t0 = float Map.Stable.V1.M(Int.Stable.V1).t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, stable_witness]

  type 'a t1 = 'a Map.Stable.V1.M(Bool.Stable.V1).t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, stable_witness]
end

module _ = struct
  type t0 = float Hashtbl.M(Int).t [@@deriving sexp, bin_io]
  type 'a t1 = 'a Hashtbl.M(Bool).t [@@deriving sexp, bin_io]
end

(* with stable witness *)
module _ = struct
  type t0 = Float.Stable.V1.t Hashtbl.Stable.V1.M(Int.Stable.V1).t
  [@@deriving bin_io, sexp, stable_witness]

  type 'a t1 = 'a Hashtbl.Stable.V1.M(Bool.Stable.V1).t
  [@@deriving bin_io, sexp, stable_witness]
end
