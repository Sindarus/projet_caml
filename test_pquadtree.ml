#use "pquadtree.ml";;
open Pqt;;

#use "unit_testing.ml";;
open UnitTesting;;

(* SELF-VALIDATED TESTS *)
let test_new_pquadtree () =
  let a = new_pquadtree 10 in
  match a with
    | PNode(p, r, pqt1, pqt2, pqt3, pqt4) ->
      if not (p = new_point 0 0) ||
         not (r = new_rect 10 0 0 10) ||
         not (pqt1 = PEmpty) ||
         not (pqt2 = PEmpty) ||
         not (pqt3 = PEmpty) ||
         not (pqt4 = PEmpty)
      then false else true
    | _ -> false
;;

let test_get_squ_num_pt () =
  let p = new_pquadtree 10 in
  if
    not ((get_squ_num_pt p (new_point 1 1)) = 3) ||
    not ((get_squ_num_pt p (new_point 9 9)) = 2) ||
    not ((get_squ_num_pt p (new_point 1 9)) = 1) ||
    not ((get_squ_num_pt p (new_point 9 1)) = 4) ||
    not ((get_squ_num_pt p (new_point 11 5)) = 0)
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

let test_insert () =
  let pqt = new_pquadtree 10 in
  let pqt = insert pqt {x=1; y=1} in
  let pqt = insert pqt {x=2; y=2} in
  let pqt = insert pqt {x=1; y=9} in
  if not (pqt =
    PNode ({x = 0; y = 0}, {top = 10; bottom = 0; right = 10; left = 0},
      PNode ({x = 1; y = 9}, {top = 10; bottom = 5; right = 5; left = 0}, PEmpty, PEmpty, PEmpty, PEmpty),
      PEmpty,
      PNode ({x = 1; y = 1}, {top = 5; bottom = 0; right = 5; left = 0},
        PEmpty,
        PNode ({x = 2; y = 2}, {top = 5; bottom = 2; right = 5; left = 2}, PEmpty, PEmpty, PEmpty, PEmpty),
        PEmpty,
        PEmpty),
      PEmpty)
  ) then false else true
;;

(* Gather tests and run them *)
let test_funcs = [
  ("test_new_pquadtree", test_new_pquadtree);
  ("test_pbelong", test_pbelong);
  ("test_ppath", test_ppath);
  ("test_new_pquadtree", test_new_pquadtree);
  ("test_insert", test_insert)
] in
run_tests test_funcs;;

(* NON SELF-VALIDATEd TESTS *)
(* to run and validate by hand *)
let test_draw_pqt () =
  let pqt = new_pquadtree 100 in
  let pqt = insert pqt {x=10; y=10} in
  let pqt = insert pqt {x=20; y=20} in
  let pqt = insert pqt {x=10; y=90} in
  draw_pqt pqt
;;
