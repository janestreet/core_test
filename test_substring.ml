open! Core
open Poly
open! Import

module Test
    (Base : sig
       type t

       val of_string : string -> t
     end)
    (Substring : Substring.S with type base = Base.t) : sig end = struct
  open Substring

  module%test [@name "get"] _ = struct
    let hello = Base.of_string "hello"

    let%test _ =
      let lo = create ~pos:3 ~len:2 hello in
      Char.equal (get lo 1) 'o'
    ;;

    let%test _ = Exn.does_raise (fun () -> get (create ~pos:1 ~len:3 hello) 3)
    let%test _ = Exn.does_raise (fun () -> get (create hello) (-1))
  end

  module%test [@name "sub"] _ = struct
    let base = Base.of_string "012345"
    let t = create ~pos:1 ~len:4 base

    (* 1234 *)

    let%test_unit _ = ignore (sub ~pos:0 ~len:4 t : t)
    let%test _ = Exn.does_raise (fun () -> sub ~pos:0 ~len:5 t)
    let%test _ = Exn.does_raise (fun () -> sub ~pos:1 ~len:4 t)

    let%test _ =
      let t2 = sub t ~pos:1 ~len:3 in
      String.init (length t2) ~f:(get t2) = "234"
    ;;

    let%test _ =
      let t2 = sub t ~pos:3 in
      String.init (length t2) ~f:(get t2) = "4"
    ;;

    let%test_unit "empty substring" = ignore (sub ~pos:2 ~len:0 t : t)
    let%test "invalid empty substring" = Exn.does_raise (fun () -> sub ~pos:5 ~len:0 t)
  end

  module%test _ = struct
    let ell = create ~pos:1 ~len:3 (Base.of_string "hello")
    let%test _ = to_array ell = [| 'e'; 'l'; 'l' |]
    let%test _ = to_list ell = [ 'e'; 'l'; 'l' ]
    let%test _ = fold ell ~init:[] ~f:(fun acc x -> x :: acc) = [ 'l'; 'l'; 'e' ]

    let%test _ =
      foldi ell ~init:[] ~f:(fun i acc x -> (i, x) :: acc) = [ 2, 'l'; 1, 'l'; 0, 'e' ]
    ;;

    let%test _ =
      let stuff = ref [] in
      iter ell ~f:(fun c -> stuff := c :: !stuff);
      !stuff = [ 'l'; 'l'; 'e' ]
    ;;

    let%test _ =
      let stuff = ref [] in
      iteri ell ~f:(fun i c -> stuff := (i, c) :: !stuff);
      !stuff = [ 2, 'l'; 1, 'l'; 0, 'e' ]
    ;;
  end

  module%test [@name "stack allocation and read-only bases"] _ = struct
    let hello_world = Base.of_string "hello world"

    [%%template
    [@@@alloc.default a @ l = (heap_global, stack_local)]
    [@@@mode.default v = (read_write, read)]

    let narrow (b @ l v) =
      (let t = (create [@alloc a] [@mode v]) ~pos:2 ~len:8 b in
       (* "llo worl" *)
       let t = (sub [@alloc a] [@mode v]) t ~pos:1 in
       (* "lo worl" *)
       let t = (prefix [@alloc a] [@mode v]) t 5 in
       (* "lo wo" *)
       let t = (suffix [@alloc a] [@mode v]) t 4 in
       (* "o wo" *)
       let t = (drop_prefix [@alloc a] [@mode v]) t 1 in
       (* " wo" *)
       (drop_suffix [@alloc a] [@mode v]) t 1)
      [@exclave_if_stack a]
    ;;

    let%test_unit _ =
      let t = (narrow [@alloc a] [@mode v]) hello_world in
      ([%test_result: string] [@alloc a]) ((to_string [@alloc a]) t) ~expect:" w";
      [%test_result: string] (Bigstring.to_string (to_bigstring t)) ~expect:" w";
      [%test_result: char] (get t 0) ~expect:' ';
      [%test_result: char] (get t 1) ~expect:'w';
      [%test_result: int] (pos t) ~expect:5;
      [%test_result: int] (length t) ~expect:2;
      [%test_result: bool] (is_empty t) ~expect:false;
      let whole = (create [@alloc a] [@mode v]) ((base [@mode l v]) t) in
      ([%test_result: string] [@alloc a])
        ((to_string [@alloc a]) whole)
        ~expect:"hello world" [@nontail]
    ;;

    let%test_unit _ =
      let t = (narrow [@alloc a] [@mode v]) hello_world in
      let check_clamped f ~expect =
        List.iter
          [ length t; length t + 1 ]
          ~f:(fun n ->
            ([%test_result: string] [@alloc a])
              ((to_string [@alloc a]) (f t n))
              ~expect [@nontail]) [@nontail]
      in
      check_clamped (prefix [@alloc a] [@mode v]) ~expect:" w";
      check_clamped (suffix [@alloc a] [@mode v]) ~expect:" w";
      check_clamped (drop_prefix [@alloc a] [@mode v]) ~expect:"";
      check_clamped (drop_suffix [@alloc a] [@mode v]) ~expect:"";
      let check_invalid_arg name f =
        match f t (-1) with
        | (_ : t) -> raise_s [%message "did not raise" (name : string)]
        | exception Invalid_argument message ->
          [%test_result: string] message ~expect:(name ^ " expecting nonnegative argument")
      in
      check_invalid_arg "prefix" (prefix [@alloc a] [@mode v]);
      check_invalid_arg "suffix" (suffix [@alloc a] [@mode v]);
      check_invalid_arg "drop_prefix" (drop_prefix [@alloc a] [@mode v]);
      check_invalid_arg "drop_suffix" (drop_suffix [@alloc a] [@mode v]) [@nontail]
    ;;]

    let%expect_test "[stack] instances do not heap-allocate" =
      let narrowed =
        require_no_allocation_local (fun () -> exclave_
          (to_string [@alloc stack]) ((narrow [@alloc stack]) hello_world))
      in
      ([%test_result: string] [@alloc stack]) narrowed ~expect:" w";
      [%expect {| |}]
    ;;
  end

  module%test _ = struct
    let bcdefghi = create ~pos:1 ~len:8 (Base.of_string "abcdefghijklmno ")
    let%test _ = find bcdefghi ~f:Char.is_lowercase = Some 'b'
    let%test _ = find bcdefghi ~f:Char.is_whitespace = None
    let%test _ = exists bcdefghi ~f:(Char.equal 'h')
    let%test _ = not (exists bcdefghi ~f:(Char.equal 'z'))
    let%test _ = for_all bcdefghi ~f:Char.is_alpha
    let%test _ = not (for_all bcdefghi ~f:(Char.equal 'h'))
    let%test _ = not (mem bcdefghi 'a')
    let%test _ = mem bcdefghi 'b'
    let%test _ = mem bcdefghi 'i'
    let%test _ = not (mem bcdefghi 'j')
    let%test _ = count bcdefghi ~f:(fun c -> String.mem "aeiou" c) = 2

    let%test _ =
      sum (module Int) bcdefghi ~f:(fun c -> Char.to_int c - Char.to_int 'a')
      = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8
    ;;

    let%test _ = min_elt bcdefghi ~compare:Char.compare = Some 'b'
    let%test _ = max_elt bcdefghi ~compare:Char.compare = Some 'i'
    let%test _ = existsi bcdefghi ~f:(fun i c -> i = 6 && Char.equal c 'h')
    let%test _ = not (existsi bcdefghi ~f:(fun i c -> i = 8 || Char.equal c 'z'))
    let%test _ = for_alli bcdefghi ~f:(fun i c -> i <= 7 && Char.is_alpha c)
    let%test _ = not (for_alli bcdefghi ~f:(fun i c -> i <= 7 && Char.equal c 'h'))

    let%test _ =
      counti bcdefghi ~f:(fun i c -> (not (String.mem "bcde" c)) && i % 2 = 0) = 2
    ;;

    let%test _ = findi bcdefghi ~f:(fun (_ : int) c -> Char.equal c 'f') = Some (4, 'f')
    let%test _ = findi bcdefghi ~f:(fun (_ : int) c -> Char.equal c 'z') = None

    let%test _ =
      find_mapi bcdefghi ~f:(fun (_ : int) c -> Option.some_if (Char.equal c 'e') 42)
      = Some 42
    ;;

    let%test _ =
      find_mapi bcdefghi ~f:(fun (_ : int) c -> Option.some_if (Char.equal c 'z') 42)
      = None
    ;;
  end
end

include Test (Bytes) (Substring)

let sexp_of_substring_for_test t =
  [%sexp
    { base = (Substring.base t : bytes)
    ; pos = (Substring.pos t : int)
    ; len = (Substring.length t : int)
    }]
;;

module%test [@name "quickcheck"] _ = struct
  let%expect_test "can generate" =
    Quickcheck.test_can_generate
      ~sexp_of:sexp_of_substring_for_test
      Substring.quickcheck_generator
      ~f:(fun sub ->
        let base_len = Bytes.length (Substring.base sub) in
        (* whole substring of nontrivial string *)
        base_len > 0 && Substring.length sub = base_len);
    Quickcheck.test_can_generate
      ~sexp_of:sexp_of_substring_for_test
      Substring.quickcheck_generator
      ~f:(fun sub ->
        let pos = Substring.pos sub in
        let len = Substring.length sub in
        (* non-triviality: contains at least one character and excludes at least one
           character from each end *)
        pos > 0 && len > 0 && pos + len < Bytes.length (Substring.base sub));
    [%expect ""]
  ;;

  let%expect_test "to_string" =
    Quickcheck.test
      ~sexp_of:sexp_of_substring_for_test
      Substring.quickcheck_generator
      ~f:(fun sub ->
        [%test_result: bytes]
          ~expect:
            (Bytes.sub
               (Substring.base sub)
               ~pos:(Substring.pos sub)
               ~len:(Substring.length sub))
          (Bytes.of_string (Substring.to_string sub));
        [%test_result: string]
          (Bigstring.to_string (Substring.to_bigstring sub))
          ~expect:(Substring.to_string sub);
        ([%test_result: string] [@alloc stack])
          ((Substring.to_string [@alloc stack]) sub)
          ~expect:(Substring.to_string sub) [@nontail])
  ;;

  let%expect_test "sexp_of_t" =
    Quickcheck.test
      ~sexp_of:sexp_of_substring_for_test
      Substring.quickcheck_generator
      ~f:(fun sub ->
        [%test_result: Sexp.t]
          ~expect:
            (Bytes.sub
               (Substring.base sub)
               ~pos:(Substring.pos sub)
               ~len:(Substring.length sub)
             |> Bytes.sexp_of_t)
          (Substring.sexp_of_t sub))
  ;;

  [%%template
  [@@@alloc.default a = (heap, stack)]
  [@@@mode.default v = (read_write, read)]

  let%expect_test "prefixes and suffixes" =
    let sub_and_n =
      let open Quickcheck.Let_syntax in
      let%bind sub = [%quickcheck.generator: Substring.t] in
      let%map n =
        (* include too-small and too-large indices *)
        Int.gen_uniform_incl (-1) (Substring.length sub + 2)
      in
      sub, n
    in
    List.iter
      [ String.drop_prefix, (Substring.drop_prefix [@alloc a] [@mode v])
      ; String.drop_suffix, (Substring.drop_suffix [@alloc a] [@mode v])
      ; String.prefix, (Substring.prefix [@alloc a] [@mode v])
      ; String.suffix, (Substring.suffix [@alloc a] [@mode v])
      ]
      ~f:(fun (f_str, f_sub) ->
        Quickcheck.test
          ~sexp_of:[%sexp_of: substring_for_test * int]
          sub_and_n
          ~f:(fun (sub, n) ->
            let str = Substring.to_string sub in
            let via_str = Option.try_with (fun () -> f_str str n) in
            let via_sub =
              Option.try_with (fun () -> Substring.to_string (f_sub sub n) [@nontail])
            in
            if Option.is_some via_str && Option.is_some via_sub
            then [%test_result: string option] ~expect:via_str via_sub))
  ;;]
end

include
  Test
    (struct
      include Bigstring

      let of_string s = of_string s
    end)
    (Bigsubstring)
