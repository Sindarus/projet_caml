#use "unit_testing.ml";;
open UnitTesting;;

#use "quadtree.ml";;
open Qtree;;

(* SELF-VALIDATED TESTS *)
let test_insert () =
  let a = new_qtree (new_rect 300 0 0 300) in
  let a = insert_rect a (new_rect 270 180 135 180) in
  let a = insert_rect a (new_rect 165 135 180 285) in
  let a = insert_rect a (new_rect 120 90 60 120) in
  let a = insert_rect a (new_rect 285 270 240 285) in
  if not (a = Q(
    new_rect 300 0 0 300, [new_rect 270 180 135 180], [new_rect 165 135 180 285],
    Empty,
    Q(
      new_rect 300 150 150 300, [], [],
      Empty,
      Q(
        new_rect 300 225 225 300, [new_rect 285 270 240 285], [],
        Empty, Empty, Empty, Empty),
      Empty,
      Empty),
    Q(
      new_rect 150 0 0 150, [new_rect 120 90 60 120], [],
      Empty, Empty, Empty, Empty),
    Empty
    )
  ) then false else true
;;

let test_rects_around_p () =
  let a = new_qtree (new_rect 300 0 0 300) in
  let a = insert_rect a (new_rect 270 180 135 180) in
  let a = insert_rect a (new_rect 237 210 30 270) in
  if not (
    (rects_around_p {x=147; y=225} a) =
    [{top = 270; bottom = 180; right = 180; left = 135};
     {top = 237; bottom = 210; right = 270; left = 30}]
  ) then false else true
;;

(* Gather tests and run them *)
let test_funcs = [
  ("test_insert", test_insert);
  ("test_rects_around_p", test_rects_around_p)
] in
run_tests test_funcs;;


(* NON SELF-VALIDATEd TESTS *)
(* to run and validate by hand *)
let test_draw_qtree () =
  let a = Q(
    new_rect 300 0 0 300, [new_rect 270 180 135 180], [new_rect 165 135 180 285],
    Empty,
    Q(
      new_rect 300 150 150 300, [], [],
      Empty,
      Q(
        new_rect 300 225 225 300, [new_rect 285 270 240 285], [],
        Empty, Empty, Empty, Empty),
      Empty,
      Empty),
    Q(
      new_rect 150 0 0 150, [new_rect 120 90 60 120], [],
      Empty, Empty, Empty, Empty),
    Empty
  ) in draw_qtree a
;; 
