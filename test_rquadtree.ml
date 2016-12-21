#use "unit_testing.ml";;
open UnitTesting;;

#use "rquadtree.ml";;
open Rqt;;

(* SELF-VALIDATED TESTS *)
let test_inverse_rqt () =
  let a = RQ(
    Plain Black,
    Plain White,
    Plain White,
    RQ(Plain White, Plain Black, Plain White, Plain Black)
  ) in
  if not (
    (inverse_rqt a) =
      RQ(
        Plain White,
        Plain Black,
        Plain Black,
        RQ(Plain Black, Plain White, Plain Black, Plain White)
      )
  )
  then false else true
;;

let test_inter_rqt () =
  let a = RQ(
    RQ(Plain Black, Plain White, Plain Black, Plain White),
    Plain White,
    Plain White,
    RQ(Plain White, Plain Black, Plain White, Plain Black)
  ) in
  let b = RQ(
    RQ(Plain White, Plain Black, Plain White, Plain Black),
    Plain White,
    Plain White,
    RQ(Plain White, Plain White, Plain Black, Plain Black)
  ) in
  if not (
    (inter_rqt a b) = RQ(
      Plain White,
      Plain White,
      Plain White,
      RQ(Plain White, Plain White, Plain White, Plain Black))
  ) then false else true
;;

let test_union_rqt () =
  let a = RQ(
    RQ(Plain Black, Plain White, Plain Black, Plain White),
    Plain White,
    Plain White,
    RQ(Plain White, Plain Black, Plain White, Plain Black)
  ) in
  let b = RQ(
    RQ(Plain White, Plain Black, Plain White, Plain Black),
    Plain White,
    Plain White,
    RQ(Plain White, Plain White, Plain Black, Plain Black)
  ) in
  if not (
    (union_rqt a b) = RQ(
      Plain Black,
      Plain White,
      Plain White,
      RQ(Plain White, Plain Black, Plain Black, Plain Black))
  ) then false else true
;;

let test_vert_sym_rqt () =
  let a = RQ(
    Plain White,
    Plain White,
    Plain White,
    RQ(Plain White, Plain Black, Plain Black, Plain Black)
  ) in
  if not (
    (vert_sym_rqt a) = RQ(
      Plain White,
      Plain White,
      RQ(Plain Black, Plain White, Plain Black, Plain Black),
      Plain White    
    )
  ) then false else true
;;

let test_horiz_sym_rqt () =
  let a = RQ(
    Plain White,
    Plain White,
    Plain White,
    RQ(Plain White, Plain Black, Plain Black, Plain Black)
  ) in
  if not (
    (horiz_sym_rqt a) = RQ(
      Plain White,
      RQ(Plain Black, Plain Black, Plain White, Plain Black),
      Plain White,
      Plain White    
    )
  ) then false else true
;;

let test_code_rqt () =
  let a = RQ(
    Plain White,
    Plain White,
    Plain White,
    RQ(Plain White, Plain Black, Plain Black, Plain Black)
  ) in
  if not ((code_rqt a) = "0101010010111111")
  then false else true
;;

let test_uncode_rqt () =
  let a = "01001101011111010111110" in
  if not(
    (uncode_rqt a) = RQ(
      Plain White,
      RQ(
        Plain Black,
        RQ(
          Plain White,
          Plain Black,
          Plain Black,
          Plain White),
        Plain White,
        Plain Black),
      Plain Black,
      Plain White)
  ) then false else true
;;

(* Gather tests and run them *)
let test_funcs = [
  ("test_inverse_rqt", test_inverse_rqt);
  ("test_inter_rqt", test_inter_rqt);
  ("test_union_rqt", test_union_rqt);
  ("test_vert_sym_rqt", test_vert_sym_rqt);
  ("test_horiz_sym_rqt", test_horiz_sym_rqt);
  ("test_code_rqt", test_code_rqt);
  ("test_uncode_rqt", test_uncode_rqt)
] in
run_tests test_funcs;;


(* NON SELF-VALIDATEd TESTS *)
(* to run and validate by hand *)
let test_draw_rqt () =
  let a = RQ(
    Plain Black,
    Plain White,
    Plain White,
    RQ(Plain White, Plain Black, Plain White, Plain Black)
  ) in
  draw_rqt a 512
;;
