#use "projet.ml";;
open Pqt;;

let test_new_pquadtree () =
  let a = new_pquadtree 10 in
  match a with
    | PNoeud(p, r, p1, p2, p3, p4) ->
      if not (p = {x = 0; y = 0}) ||
         not (r = {left=0; bottom=0; top=10; right=10}) ||
         not (p1 = PEmpty) ||
         not (p2 = PEmpty) ||
         not (p3 = PEmpty) ||
         not (p4 = PEmpty)
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
    PNoeud(
      {x = 0; y = 0},
      {top = 10; bottom = 0; right = 10; left = 0},
      PNoeud(
        {x = 3; y = 9},
        {top = 10; bottom = 5; right = 5; left = 0},
        PEmpty, PEmpty, PEmpty, PEmpty),
      PEmpty,
      PNoeud(
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
    PNoeud(
      {x = 0; y = 0},
      {top = 10; bottom = 0; right = 10; left = 0},
      PNoeud(
        {x = 3; y = 9},
        {top = 10; bottom = 5; right = 5; left = 0},
        PEmpty, PEmpty, PEmpty, PEmpty),
      PEmpty,
      PNoeud(
        {x = 1; y = 2},
        {top = 5; bottom = 0; right = 5; left = 0},
        PEmpty, PEmpty, PEmpty, PEmpty),
      PEmpty
    ) in
  if
    not ((ppath p {x=3; y=9}) = ["NO"]) ||
    not ((ppath p {x=1; y=2}) = ["SO"]) ||
    not ((ppath p {x=0; y=0}) = [])
  then false else true
;;


let run_test_func func =
  if func () then print_string "OK\n" else print_string "NOT OK\n"
;;

let run_tests () =
  print_string "test_new_pquadtree : ";
  run_test_func test_new_pquadtree;
  print_string "test_get_center : ";
  run_test_func test_get_center;
  print_string "test_out_rect : ";
  run_test_func test_out_rect;
  print_string "test_get_squ_num_pt : ";
  run_test_func test_get_squ_num_pt;
  print_string "test_pbelong : ";
  run_test_func test_pbelong;
  print_string "test_ppath : ";
  run_test_func test_ppath
;;

