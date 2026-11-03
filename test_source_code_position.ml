open! Core
open! Import
open! Source_code_position.With_hiding

let%expect_test "[to_string]" =
  print_endline ~hide_paths:true (to_string [%here]);
  [%expect {| test_source_code_position.ml:LINE:COL |}]
;;

let%expect_test "[sexp_of_t]" =
  print_s ~hide_paths:true [%sexp ([%here] : t)];
  [%expect {| test_source_code_position.ml:LINE:COL |}]
;;
