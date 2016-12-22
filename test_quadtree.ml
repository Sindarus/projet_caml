#use "unit_testing.ml";;
open UnitTesting;;

#use "quadtree.ml";;
open Qtree;;

(* SELF-VALIDATED TESTS *)


(* Gather tests and run them *)
let test_funcs = [
  (* ("", ); *)
] in
run_tests test_funcs;;


(* NON SELF-VALIDATEd TESTS *)
(* to run and validate by hand *)
let test_draw_qtree =
  let a = Q(
    new_rect 300 0 0 300, [new_rect 270 180 135 180], [new_rect 165 135 180 285],
    Empty,
    Q(
      new_rect 300 150 150 300, [], [],
      Empty,
      Q(
        new_rect 300 225 225 300, [new_rect 285 270 240 285], [],
        Empty, Empty, Empty, Empty ),
      Empty,
      Empty),
    Q(
      new_rect 150 0 0 150, [new_rect 120 90 60 120], [],
      Empty, Empty, Empty, Empty),
    Empty
  ) in draw_qtree a
;; 
