open Core

module type%template [@mode m = (local, global)] S = sig
  type t [@@deriving sexp_of, (compare [@mode.explicit m])]
end

module type Examples = sig
  type t

  val examples : t list
end

module%template
  [@mode m = (local, global)]
  Make
    (Input : S
  [@mode m])
    (Output : S
  [@mode m])
    (Examples : Examples with type t := Input.t) =
struct
  open Examples

  type t = Input.t -> Output.t

  let to_alist (f @ local) = List.map examples ~f:(fun x -> x, f x) [@nontail]

  let%template[@mode n = (global, m)] compare (t1 @ n) (t2 @ n) =
    ([%compare: (Input.t * Output.t) list] [@mode.explicit n]) (to_alist t1) (to_alist t2)
  ;;

  let sexp_of_t t = [%sexp (to_alist t : (Input.t * Output.t) list)]
end
