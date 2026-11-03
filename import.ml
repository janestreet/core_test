open! Core
include Expect_test_helpers_core
module Blit_helpers = Core_test_helpers.Blit_helpers
module Test_container = Base_test_helpers.Test_container
module Variant = Variantslib.Variant
module Float_u = Float
module Int32_u = Int32
module Int64_u = Int64
module Nativeint_u = Nativeint
module Mutex = Capsule.Blocking_sync.Mutex [@@alert "-deprecated"]

let () = Dynamic.set_root Sexp.of_int_style `Underscores
