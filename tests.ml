#use "projet.ml";;
open Pqt;;

let test_new_pquadtree =
  let a = new_pquadtree 10 in
  match a with
    | PNoeud(p, r, p1, p2, p3, p4) ->
      if
        not (p = {x = 0; y = 0}) ||
        not (r = {left=0; bottom=0; top=10; right=10}) ||
        not (p1 = PEmpty) ||
        not (p2 = PEmpty) ||
        not (p3 = PEmpty) ||
        not (p4 = PEmpty)
      then false else true
    | _ -> false
;;

let test_get_center =
  let r = {left=0; bottom=0; top=10; right=10} in
  let c = get_center r in
  if not (c = {x = 5; y = 5}) then false else true
;;

let test_out_rect =
  let r = {left=0; bottom=0; top=10; right=10} in
  if
    not (out_rect r {x=(-1); y=0})
    || not (out_rect r {x=10; y=11})
  then false else true
;;

let test_get_squ_num_pt =
  false
;;


let run_test_func func =
  if func then print_string "OK\n" else print_string "NOT OK\n"
;;

let run_tests () =
  begin
    print_string "test_new_pquadtree : ";
    run_test_func test_new_pquadtree;
    print_string "test_get_center : ";
    run_test_func test_get_center;
    print_string "test_out_rect : ";
    run_test_func test_out_rect;
    print_string "test_get_squ_num_pt : ";
    run_test_func test_get_squ_num_pt
  end
;;

