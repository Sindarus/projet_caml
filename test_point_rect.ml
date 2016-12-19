#use "unit_testing.ml";;
open UnitTesting;;

#use "point_rect.ml";;
open PointRect;;

(* SELF-VALIDATED TESTS *)
let test_get_center () =
  let r = new_rect 20 10 10 20 in
  let c = get_center r in
  if not (c = new_point 15 15) then false else true
;;

let test_out_rect () =
  let r = new_rect 10 0 0 10 in
  if
    not (out_rect r (new_point (-1) 0)) ||
    not (out_rect r (new_point 10 11))
  then false else true
;;

let test_get_rect_squ () =
  let r = {bottom=0; left=0; right=10; top=10} in
  if
    not ((get_rect_squ r 1) = {top = 10; bottom = 5; right = 5; left = 0}) ||
    not ((get_rect_squ r 2) = {top = 10; bottom = 5; right = 10; left = 5}) ||
    not ((get_rect_squ r 3) = {top = 5; bottom = 0; right = 5; left = 0})
  then false else true
;;

(* Gather tests and run them *)
let test_funcs = [
  ("test_get_center", test_get_center);
  ("test_out_rect", test_out_rect);
  ("test_get_rect_squ", test_get_rect_squ);
] in
run_tests test_funcs;;