open! Core

module type%template [@mode m = (local, global)] S = sig
  type t [@@deriving sexp_of, (compare [@mode.explicit m])]
end

module type Examples = sig
  type t

  val examples : t list
end

(** Provides [sexp_of, compare] for function types. Both work by first converting
    functions to a [(Input.t, Output.t) List.Assoc.t] using [Examples.examples] as the
    inputs. The resulting sexps and comparisons will only be complete with respect to the
    given [Examples.examples]. *)
module%template
  [@mode m = (local, global)]
  Make
    (Input : S
  [@mode m])
    (Output : S
  [@mode m])
    (Examples : Examples with type t := Input.t) :
  S [@mode m] with type t = Input.t -> Output.t
