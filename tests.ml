#use "projet.ml";;
open Pqt;;

let test_new_pquadtree () =
  let a = new_pquadtree 10 in
  match a with
    | PNode(p, r, pqt1, pqt2, pqt3, pqt4) ->
      if not (p = {x = 0; y = 0}) ||
         not (r = {left=0; bottom=0; top=10; right=10}) ||
         not (pqt1 = PEmpty) ||
         not (pqt2 = PEmpty) ||
         not (pqt3 = PEmpty) ||
         not (pqt4 = PEmpty)
      then false else true
    | _ -> false
;;

let test_get_center () =
  let r = {left=0; bottom=0; top=10; right=10} in
  let c = get_center r in
  if not (c = {x = 5; y = 5}) then false else true
;;

let test_out_rect () =
  let r = {left=0; bottom=0; top=10; right=10} in
  if
    not (out_rect r {x=(-1); y=0}) ||
    not (out_rect r {x=10; y=11})
  then false else true
;;

let test_get_squ_num_pt () =
  let p = new_pquadtree 10 in
  if
    not ((get_squ_num_pt p {x=1; y=1}) = 3) ||
    not ((get_squ_num_pt p {x=9; y=9}) = 2) ||
    not ((get_squ_num_pt p {x=1; y=9}) = 1) ||
    not ((get_squ_num_pt p {x=9; y=1}) = 4) ||
    not ((get_squ_num_pt p {x=11; y=5}) = 0)
  then false else true
;;

let test_pbelong () =
  let p =
    PNode(
      {x = 0; y = 0},
      {top = 10; bottom = 0; right = 10; left = 0},
      PNode(
        {x = 3; y = 9},
        {top = 10; bottom = 5; right = 5; left = 0},
        PEmpty, PEmpty, PEmpty, PEmpty),
      PEmpty,
      PNode(
        {x = 1; y = 2},
        {top = 5; bottom = 0; right = 5; left = 0},
        PEmpty, PEmpty, PEmpty, PEmpty),
      PEmpty
    ) in
  if
    not (pbelong p {x=0; y=0}) ||
    not (pbelong p {x=3; y=9}) ||
    not (pbelong p {x=1; y=2}) ||
    pbelong p {x=9; y=9}
  then false else true
;;

let test_ppath () =
  let p =
    PNode(
      {x = 0; y = 0},
      {top = 10; bottom = 0; right = 10; left = 0},
      PNode(
        {x = 3; y = 9},
        {top = 10; bottom = 5; right = 5; left = 0},
        PEmpty, PEmpty, PEmpty, PEmpty),
      PEmpty,
      PNode(
        {x = 1; y = 2},
        {top = 5; bottom = 0; right = 5; left = 0},
        PEmpty, PEmpty, PEmpty, PEmpty),
      PEmpty
    ) in
  if
    not ((ppath p {x=3; y=9}) = ["NW"]) ||
    not ((ppath p {x=1; y=2}) = ["SW"]) ||
    not ((ppath p {x=0; y=0}) = [])
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


let run_test_func name_and_func =
  let (func_name, func) = name_and_func in
  print_string func_name;
  print_string ": ";
  if func ()
  then begin print_string "OK\n"; true end
  else begin print_string "NOT OK\n"; false end
;;

let run_tests () =
  let test_funcs = [
    ("test_new_pquadtree", test_new_pquadtree);
    ("test_get_center", test_get_center);
    ("test_out_rect", test_out_rect);
    ("test_get_squ_num_pt", test_get_squ_num_pt);
    ("test_pbelong", test_pbelong);
    ("test_ppath", test_ppath);
    ("test_new_pquadtree", test_new_pquadtree)
  ] in
  List.map run_test_func test_funcs
;;
