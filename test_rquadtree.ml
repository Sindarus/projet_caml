#use "unit_testing.ml";;
open UnitTesting;;

#use "rquadtree.ml";;
open Rqt;;

(* SELF-VALIDATED TESTS *)
let test_inverse_rqt () =
  let a = RQ(
    Uni Black,
    Uni White,
    Uni White,
    RQ(Uni White, Uni Black, Uni White, Uni Black)
  ) in
  if not (
    (inverse_rqt a) =
      RQ(
        Uni White,
        Uni Black,
        Uni Black,
        RQ(Uni Black, Uni White, Uni Black, Uni White)
      )
  )
  then false else true
;;

let test_inter_rqt () =
  let a = RQ(
    RQ(Uni Black, Uni White, Uni Black, Uni White),
    Uni White,
    Uni White,
    RQ(Uni White, Uni Black, Uni White, Uni Black)
  ) in
  let b = RQ(
    RQ(Uni White, Uni Black, Uni White, Uni Black),
    Uni White,
    Uni White,
    RQ(Uni White, Uni White, Uni Black, Uni Black)
  ) in
  if not (
    (inter_rqt a b) = RQ(
      Uni White,
      Uni White,
      Uni White,
      RQ(Uni White, Uni White, Uni White, Uni Black))
  ) then false else true
;;

let test_union_rqt () =
  let a = RQ(
    RQ(Uni Black, Uni White, Uni Black, Uni White),
    Uni White,
    Uni White,
    RQ(Uni White, Uni Black, Uni White, Uni Black)
  ) in
  let b = RQ(
    RQ(Uni White, Uni Black, Uni White, Uni Black),
    Uni White,
    Uni White,
    RQ(Uni White, Uni White, Uni Black, Uni Black)
  ) in
  if not (
    (union_rqt a b) = RQ(
      Uni Black,
      Uni White,
      Uni White,
      RQ(Uni White, Uni Black, Uni Black, Uni Black))
  ) then false else true
;;

let test_vert_sym_rqt () =
  let a = RQ(
    Uni White,
    Uni White,
    Uni White,
    RQ(Uni White, Uni Black, Uni Black, Uni Black)
  ) in
  if not (
    (vert_sym_rqt a) = RQ(
      Uni White,
      Uni White,
      RQ(Uni Black, Uni White, Uni Black, Uni Black),
      Uni White    
    )
  ) then false else true
;;

let test_horiz_sym_rqt () =
  let a = RQ(
    Uni White,
    Uni White,
    Uni White,
    RQ(Uni White, Uni Black, Uni Black, Uni Black)
  ) in
  if not (
    (horiz_sym_rqt a) = RQ(
      Uni White,
      RQ(Uni Black, Uni Black, Uni White, Uni Black),
      Uni White,
      Uni White    
    )
  ) then false else true
;;

let test_code_rqt () =
  let a = RQ(
    Uni White,
    Uni White,
    Uni White,
    RQ(Uni White, Uni Black, Uni Black, Uni Black)
  ) in
  if not ((code_rqt a) = "0101010010111111")
  then false else true
;;

let test_uncode_rqt () =
  let a = "01001101011111010111110" in
  if not(
    (uncode_rqt a) = RQ(
      Uni White,
      RQ(
        Uni Black,
        RQ(
          Uni White,
          Uni Black,
          Uni Black,
          Uni White),
        Uni White,
        Uni Black),
      Uni Black,
      Uni White)
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
    Uni Black,
    Uni White,
    Uni White,
    RQ(Uni White, Uni Black, Uni White, Uni Black)
  ) in
  draw_rqt a 512
;;
