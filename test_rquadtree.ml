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

(* Gather tests and run them *)
let test_funcs = [
  ("test_inverse_rqt", test_inverse_rqt)
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
