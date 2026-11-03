open! Core
open! Import

(* Check that the Or_error.t generator is indeed hitting both cases with non-trivial
   frequency. Out of 500 coin flips, the probability of getting <125 or >375 heads is
   about 2e-30. *)
let%test_unit "generator" =
  let generator = [%quickcheck.generator: int Or_error.t] in
  Base_quickcheck.Test.with_sample_exn generator ~f:(fun sequence ->
    let num_ok = Sequence.take sequence 500 |> Sequence.count ~f:Or_error.is_ok in
    assert (125 <= num_ok && num_ok <= 375))
;;

module%test Portable = struct
  (* [test] checks that [A] and [B] are wire-compatible:
     - They share the same bin_digest, which on its own gives us some confidence the
       serializers/deserializers are compatible.
     - The [A.t] values round-trip through the bin-io serializers and deserializers. More
       specifically, both occurrences of "bytes" in this diagram are the same:
       [A.t -> bytes -> B.t -> bytes]
  *)
  let test
    (type a b)
    (module A : Stable_without_comparator with type t = a)
    (module B : Stable_without_comparator with type t = b)
    examples_a
    ~expect
    =
    Expect_test_helpers_core.print_and_check_stable_type (module A) examples_a;
    expect ();
    let examples_b =
      List.map examples_a ~f:(fun a ->
        Binable.of_string (module B) (Binable.to_string (module A) a))
    in
    Expect_test_helpers_core.print_and_check_stable_type (module B) examples_b;
    expect ()
  ;;

  let examples = [ Ok 13; Or_error.error_string "error" ]

  module%template
    [@mode m = (local, global)]
    Instantiate (Stable : sig
      type 'a t [@@deriving bin_io, (compare [@mode.explicit m]), sexp]
    end) =
  struct
    type t = int Stable.t [@@deriving bin_io, (compare [@mode.explicit m]), sexp]
  end

  let%expect_test "or_error -> portable" =
    test
      (module Instantiate [@mode local] (Or_error.Stable.V1))
      (module Instantiate (Or_error.Stable.Portable.V1))
      examples
      ~expect:(fun () ->
        [%expect
          {|
          (bin_shape_digest f8199442eda207379cbc7b5e47529fe8)
          ((sexp (Ok 13)) (bin_io "\000\r"))
          ((sexp (Error error)) (bin_io "\001\000\005error"))
          |}]);
    test
      (module Instantiate (Or_error.Stable.V2))
      (module Instantiate (Or_error.Stable.Portable.V2))
      examples
      ~expect:(fun () ->
        [%expect
          {|
          (bin_shape_digest ce7928a05bd6e1c5ac297c653c6aafa2)
          ((sexp (Ok 13)) (bin_io "\000\r"))
          ((sexp (Error error)) (bin_io "\001\001\005error"))
          |}])
  ;;

  let%expect_test "portable -> or_error" =
    let examples = [ Ok 13; Or_error.error_string "error" |> Or_error.portabilize ] in
    test
      (module Instantiate (Or_error.Stable.Portable.V1))
      (module Instantiate [@mode local] (Or_error.Stable.V1))
      examples
      ~expect:(fun () ->
        [%expect
          {|
          (bin_shape_digest f8199442eda207379cbc7b5e47529fe8)
          ((sexp (Ok 13)) (bin_io "\000\r"))
          ((sexp (Error error)) (bin_io "\001\000\005error"))
          |}]);
    test
      (module Instantiate (Or_error.Stable.Portable.V2))
      (module Instantiate (Or_error.Stable.V2))
      examples
      ~expect:(fun () ->
        [%expect
          {|
          (bin_shape_digest ce7928a05bd6e1c5ac297c653c6aafa2)
          ((sexp (Ok 13)) (bin_io "\000\r"))
          ((sexp (Error error)) (bin_io "\001\001\005error"))
          |}])
  ;;
end
