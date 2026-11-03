open! Core
open! Import
open! Binable

let%expect_test "int bin shape" =
  print_endline [%bin_digest: int];
  [%expect {| 698cfa4093fe5e51523842d37b92aeac |}]
;;

let%expect_test "int32 bin shape" =
  print_endline [%bin_digest: Int32.t];
  [%expect {| 0892f5f3797659e9ecf8a0faa5f76829 |}]
;;

let%expect_test "int64 bin shape" =
  print_endline [%bin_digest: Int64.t];
  [%expect {| 0078f5c24ad346a7066cb6673cd5c3cb |}]
;;

let%expect_test "nativeint bin shape" =
  print_endline [%bin_digest: Nativeint.t];
  [%expect {| 48d60b2896ac632fd68e45fccd6774ab |}]
;;

let%expect_test "float bin shape" =
  print_endline [%bin_digest: float];
  [%expect {| 1fd923acb2dd9c5d401ad5b08b1d40cd |}]
;;

let%test_unit _ =
  let module M = struct
    type t = int [@@deriving bin_io]
  end
  in
  let m : int m = (module M) in
  List.iter
    [ Int.min_value; Int.min_value / 2; -1; 0; 1; Int.max_value / 2; Int.max_value ]
    ~f:(fun i ->
      let check name of_x to_x =
        let i' = of_x m (to_x m i) in
        if i <> i'
        then
          Error.failwiths
            (Printf.sprintf "Binable.{of,to}_%s failure" name)
            (i, `Round_tripped_to i')
            [%sexp_of: int * [ `Round_tripped_to of int ]]
      in
      check "string" of_string to_string;
      check "bigstring" of_bigstring to_bigstring)
;;

let%test_unit "Of_sexpable" =
  let module M = struct
    type t = int

    include Of_sexpable_without_uuid [@alert "-legacy"] (struct
        type t = int [@@deriving sexp]
      end)
  end
  in
  let m = (module M : S with type t = M.t) in
  List.iter [ Int.min_value; -1; 0; 1; Int.max_value ] ~f:(fun int ->
    [%test_result: int] ~expect:int (of_string m (to_string m int)))
;;

let%test_unit "type class conversions" =
  let module M = struct
    type t =
      { int : int
      ; string : string
      }
    [@@deriving bin_io, compare, equal, sexp_of]
  end
  in
  let value = { M.int = 13; string = "thirteen" } in
  let m = m_of_type_class M.bin_t in
  let type_class = m_to_type_class m in
  let round_tripped =
    Bin_prot.Writer.to_string type_class.writer value
    |> Bin_prot.Reader.of_string type_class.reader
  in
  [%test_result: M.t] ~expect:value round_tripped;
  let module Round_tripped_m = (val (m_of_type_class type_class : M.t m)) in
  [%test_result: M.t]
    ~expect:value
    (of_string (module Round_tripped_m) (to_string m value))
;;

let%expect_test "of_bigstring fails if the buffer is too long or too short" =
  let good_bigstring = to_bigstring (module String) "test value" in
  print_endline (Bigstring.Hexdump.to_string_hum good_bigstring);
  [%expect
    {| 00000000  0a 74 65 73 74 20 76 61  6c 75 65                 |.test value| |}];
  print_endline (of_bigstring (module String) good_bigstring);
  [%expect {| test value |}];
  (* if the bigstring is too short, the [bin_read_t] function will raise: *)
  let shorter_bigstring = Bigstring.sub_shared ~len:10 good_bigstring in
  require_does_raise (fun () -> of_bigstring (module String) shorter_bigstring);
  [%expect {| (Bin_prot__Common.Buffer_short) |}];
  (* if the bigstring is too long, the [bin_read_t] function will not consume all of it,
     and [of_bigstring] will raise. *)
  let longer_bigstring = Bigstring.create 30 in
  Bigstring.memset longer_bigstring ~pos:0 ~len:30 '\xff';
  Bigstring.blit
    ~src:good_bigstring
    ~src_pos:0
    ~dst:longer_bigstring
    ~dst_pos:0
    ~len:(Bigstring.length good_bigstring);
  require_does_raise (fun () -> of_bigstring (module String) longer_bigstring);
  [%expect
    {|
    ("bin_read_t did not consume the entire buffer"
     (consumed         11)
     (bigstring_length 30))
    |}]
;;

let%test_unit "Core_bin_prot" =
  let module Foo = struct
    type t =
      { a : int
      ; b : float
      ; c : string
      }
    [@@deriving bin_io, equal]

    let create i = { a = i; b = float i; c = Int.to_string i }
  end
  in
  let foo = Foo.create 1 in
  assert (
    Bin_prot.Writer.to_string Foo.bin_writer_t foo
    |> Bin_prot.Reader.of_string Foo.bin_reader_t
    |> Foo.equal foo);
  assert (
    Bin_prot.Writer.to_bytes Foo.bin_writer_t foo
    |> Bin_prot.Reader.of_bytes Foo.bin_reader_t
    |> Foo.equal foo);
  assert (
    Bin_prot.Writer.to_bigstring Foo.bin_writer_t foo
    |> Bin_prot.Reader.of_bigstring Foo.bin_reader_t
    |> Foo.equal foo)
;;
