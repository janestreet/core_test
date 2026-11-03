open! Core
open! Import

module Expect_test_config = struct
  include Expect_test_config

  (* Make error messages between [Set_once] and [Set_once.Atomic] align textually. *)
  let sanitize str =
    let replaced =
      String.substr_replace_all str ~pattern:"Set_once.Atomic" ~with_:"Set_once"
    in
    let formatted =
      Sexp.of_string_many replaced
      |> List.map ~f:Expect_test_helpers_base.sexp_to_string
      |> String.concat ~sep:"\n"
    in
    formatted
  ;;
end

module Test (Set_once : sig
    [@@@implicit_kind: ('a : value mod contended portable)]

    type 'a t [@@deriving sexp_of]

    val create : unit -> _ t
    val invariant : ('a -> unit) -> ('a t -> unit)
    val get : 'a t -> 'a option
    val get_exn : here:[%call_pos] -> 'a t -> 'a
    val set : 'a t -> here:[%call_pos] -> 'a -> unit Or_error.t
    val set_exn : 'a t -> here:[%call_pos] -> 'a -> unit
    val set_if_none : 'a t -> here:[%call_pos] -> 'a -> unit
    val is_none : _ t -> bool
    val is_some : _ t -> bool
    val iter : 'a t -> f:('a -> unit) @ local -> unit
  end) =
struct
  open Set_once

  type t = int Set_once.t [@@deriving sexp_of]

  let hide_positions = true
  let hide_paths = true

  let show t =
    print_s ~hide_positions ~hide_paths [%message "" ~_:(t : t)];
    invariant ignore t
  ;;

  let%expect_test "[sexp_of_t]" =
    let t = create () in
    show t;
    [%expect {| unset |}];
    set_exn t ~here:[%here] 13;
    show t;
    [%expect
      {|
      ((value  13)
       (set_at test_set_once.ml:LINE:COL))
      |}]
  ;;

  let%expect_test "handles [Lexing.dummy_pos]" =
    (* Location information should be excluded when [here] is [Lexing.dummy_pos], which is
       the default value of [here] in the external version of Base *)
    let t = create () in
    show_raise (fun () -> get_exn t ~here:Lexing.dummy_pos);
    [%expect {| (raised "[Set_once.get_exn] unset") |}];
    set_exn t ~here:Lexing.dummy_pos 12;
    show t;
    [%expect {| (value 12) |}]
  ;;

  let%expect_test "[get]" =
    let t = create () in
    let show_get () = print_s [%message "" ~_:(get t : int option)] in
    show_get ();
    [%expect {| () |}];
    set_exn t 13;
    show_get ();
    [%expect {| (13) |}]
  ;;

  let%expect_test "[get] doesn't allocate" =
    let t = create () in
    let check_get here =
      ignore (require_no_allocation ~here (fun () -> get t) : int option)
    in
    check_get [%here];
    [%expect {| |}];
    set_exn t 13;
    check_get [%here];
    [%expect {| |}]
  ;;

  let%expect_test "[get_exn]" =
    let t = create () in
    show_raise ~hide_positions ~hide_paths (fun () -> get_exn ~here:[%here] t);
    [%expect {| (raised ("[Set_once.get_exn] unset" (at test_set_once.ml:LINE:COL))) |}];
    set_exn t ~here:[%here] 13;
    print_s [%message "" ~_:(get_exn t : int)];
    [%expect {| 13 |}]
  ;;

  let%expect_test "[set]" =
    let t = create () in
    print_s [%message "" ~_:(set t 13 : unit Or_error.t)];
    [%expect {| (Ok ()) |}]
  ;;

  let%expect_test "[set_if_none]" =
    let t = create () in
    let set_if_none_and_print ~value =
      set_if_none t value;
      print_s ~hide_positions [%sexp (Set_once.get_exn t : string)]
    in
    set_if_none_and_print ~value:"first call to set_if_none";
    [%expect {| "first call to set_if_none" |}];
    set_if_none_and_print ~value:"second call to set_if_none";
    [%expect {| "first call to set_if_none" |}]
  ;;

  let%expect_test "[set] error" =
    let t = create () in
    set_exn t ~here:[%here] 13;
    print_s
      ~hide_positions
      ~hide_paths
      [%message "" ~_:(set t ~here:[%here] 14 : unit Or_error.t)];
    [%expect
      {|
      (Error (
        "[Set_once.set_exn] already set"
        (setting_at        test_set_once.ml:LINE:COL)
        (previously_set_at test_set_once.ml:LINE:COL)))
      |}]
  ;;

  let%expect_test "[set_exn] error" =
    let t = create () in
    set_exn t ~here:[%here] 13;
    show_raise ~hide_positions ~hide_paths (fun () -> set_exn t ~here:[%here] 14);
    [%expect
      {|
      (raised (
        "[Set_once.set_exn] already set"
        (setting_at        test_set_once.ml:LINE:COL)
        (previously_set_at test_set_once.ml:LINE:COL)))
      |}]
  ;;

  let%expect_test "[is_none], [is_some]" =
    let t = create () in
    let show () =
      print_s [%message "" ~is_none:(is_none t : bool) ~is_some:(is_some t : bool)]
    in
    show ();
    [%expect
      {|
      ((is_none true)
       (is_some false))
      |}];
    set_exn t 13;
    show ();
    [%expect
      {|
      ((is_none false)
       (is_some true))
      |}]
  ;;

  let%expect_test "[iter]" =
    let t = create () in
    let iter () = iter t ~f:(fun i -> print_s [%message (i : int)]) in
    iter ();
    [%expect {| |}];
    set_exn t 13;
    iter ();
    [%expect {| (i 13) |}]
  ;;
end

module _ = Test (Set_once)
module _ = Test (Set_once.Atomic)

(* The following functions aren't defined for [Set_once.Atomic] *)

open Set_once

module type Format = sig
  type 'a t = 'a Set_once.t [@@deriving bin_io, sexp]
end

let test_serialization (module Format : Format) =
  let module T = struct
    type t = int Format.t [@@deriving bin_io, sexp]

    let compare t1 t2 = Option.compare [%compare: int] (Set_once.get t1) (Set_once.get t2)
  end
  in
  let t1 = Set_once.create () in
  let t2 = Set_once.create () in
  Set_once.set_exn t2 13;
  print_and_check_stable_type (module T) [ t1; t2 ]
;;

let%expect_test "[Stable.V1] serialization" =
  test_serialization (module Set_once.Stable.V1);
  [%expect
    {|
    (bin_shape_digest 8b7c356301db5206ab98e334f4886c11)

    ((sexp ()) (bin_io "\000"))

    ((sexp (13)) (bin_io "\001\r"))
    |}]
;;

let%expect_test "[Unstable] serialization" =
  test_serialization (module Set_once.Unstable);
  [%expect
    {|
    (bin_shape_digest 8b7c356301db5206ab98e334f4886c11)

    ((sexp ()) (bin_io "\000"))

    ((sexp (13)) (bin_io "\001\r"))
    |}]
;;

let%expect_test "[match%optional]" =
  let t = create () in
  let show () =
    print_s
      (let open Optional_syntax in
       match%optional t with
       | None -> [%message "none"]
       | Some i -> [%message "some" ~_:(i : int)])
  in
  show ();
  [%expect {| none |}];
  set_exn t 13;
  show ();
  [%expect {| (some 13) |}]
;;

let%expect_test "[get_or_set_thunk] already set" =
  let t = create () in
  set_exn t 13;
  let return_14 () =
    print_endline "f unexpectedly called";
    14
  in
  print_s
    [%message
      "Already set, return existing value" (get_or_set_thunk t ~f:return_14 : int)];
  [%expect
    {| ("Already set, return existing value" ("get_or_set_thunk t ~f:return_14" 13)) |}]
;;

let%expect_test "[get_or_set_thunk] not set" =
  let t = create () in
  let return_14 () =
    print_endline "f expectedly called";
    14
  in
  print_s
    [%message
      "Not set, should call f and return value:" (get_or_set_thunk t ~f:return_14 : int)];
  print_s [%message "Value should be set: " (get t : int option)];
  [%expect
    {|
    f

    expectedly

    called

    ("Not set, should call f and return value:" (
      "get_or_set_thunk t ~f:return_14" 14))

    ("Value should be set: " ("get t" (14)))
    |}]
;;
