#load "graphics.cma";;
open Graphics;;

#use "point_rect.ml";;
open Point_rect;;

module Pqt =
  struct
    type pquadtree =
      | PEmpty
      | PNode of point * rect * pquadtree * pquadtree * pquadtree * pquadtree

    (* Returns a pquadtree that has n*n for support rectangle *)
    let new_pquadtree = fun n ->
      PNode(new_point 0 0,
            new_rect n 0 0 n,
            PEmpty, PEmpty, PEmpty, PEmpty)

    (* Returns x to the power p *)
    let rec pow x p =
    match p with
        | 0 -> 1
        | i when i > 0 -> x * (pow x (p-1))
        | _ -> failwith "Invalid_Input"
    ;;

    (* Returns a pquadtree that has 2^k x 2^k support rectangle *)
    let new_pquadtree_pow2 = fun k ->
      new_pquadtree (pow 2 k)
    ;;

    (* From a pquadtree and a point, this function returns the number of the
       square in which the point is.
       This does not mean that the pquadtree has been splitted yet :
       pqt1, pqt2, pqt3 et pqt4 can be uninitialized.
       This function returns 0 if the point cannot be contained by the tree.
       That means 1) when tree is PEmpty 2) when point is outside of tree's
       support rectangle *)
    let get_squ_num_pt = fun tree point ->
      match tree with
        | PEmpty -> 0
        | PNode(p, r, pqt1, pqt2, pqt3, pqt4) when (out_rect r point) -> 0
        | PNode(p, r, pqt1, pqt2, pqt3, pqt4) ->
          let center = (get_center r) in
          if point.x < center.x
          then
            (if point.y < center.y then 3 else 1)
          else
            (if point.y < center.y then 4 else 2)

    (* Returns true if the point is a value of tree.
       Careful : this is different from knowing whether the point is supported
       by the tree's support rectangle *)
    let rec pbelong = fun tree point ->
      match tree with
        | PEmpty -> false
        | PNode(p, r, pqt1, pqt2, pqt3, pqt4) when p = point -> true
        | PNode(p, r, pqt1, pqt2, pqt3, pqt4) ->
          let cadr_num = get_squ_num_pt tree point in match cadr_num with
            | 0 -> false
            | 1 -> pbelong pqt1 point
            | 2 -> pbelong pqt2 point
            | 3 -> pbelong pqt3 point
            | 4 -> pbelong pqt4 point
            | _ -> failwith "pbelong: Not possible"

    (* Returns the list of squares you have to go through in order to reach
       point, given that point is a value of tree. *)
    let rec ppath = fun tree point ->
      match tree with
        | PEmpty -> failwith "ppath: point is not in tree"
        | PNode(p, r, pqt1, pqt2, pqt3, pqt4) when p = point -> []
        | PNode(p, r, pqt1, pqt2, pqt3, pqt4) ->
          let cadr_num = get_squ_num_pt tree point in match cadr_num with
            | 0 -> failwith "ppath: point is not in tree 2"
            | 1 -> "NW" :: (ppath pqt1 point)
            | 2 -> "NE" :: (ppath pqt2 point)
            | 3 -> "SW" :: (ppath pqt3 point)
            | 4 -> "NE" :: (ppath pqt4 point)
            | _ -> failwith "ppath: not possible"

    (* Returns an empty node that has p for point and rect for support rectangle *)
    let new_node = fun p r ->
      PNode(p, r, PEmpty, PEmpty, PEmpty, PEmpty)

    (* Given a tree that is not PEmpty and a point, this function splits the deepest square
       in tree whose support rectangle supports point, by adding a pquadtree there,
       that has point for value *)
    let rec insert = fun tree point ->
      match tree with
        | PEmpty -> failwith "insert: tree is empty"
        | PNode(p, r, pqt1, pqt2, pqt3, pqt4) when p = point ->
          failwith "insert: point is already in tree"
        | PNode(p, r, pqt1, pqt2, pqt3, pqt4) ->
          let cadr_num = (get_squ_num_pt tree point) in match cadr_num with
            | 0 -> failwith "insert: point is not supported by tree's support rectangle"
            | 1 when pqt1 = PEmpty -> PNode(p, r, new_node (point) (get_rect_squ r 1), pqt2, pqt3, pqt4)
            | 1 -> PNode(p, r, insert pqt1 point, pqt2, pqt3, pqt4)
            | 2 when pqt2 = PEmpty -> PNode(p, r, pqt1, new_node (point) (get_rect_squ r 2), pqt3, pqt4)
            | 2 -> PNode(p, r, pqt1, insert pqt2 point, pqt3, pqt4)
            | 3 when pqt3 = PEmpty -> PNode(p, r, pqt1, pqt2, new_node (point) (get_rect_squ r 3), pqt4)
            | 3 -> PNode(p, r, pqt1, pqt2, insert pqt3 point, pqt4)
            | 4 when pqt4 = PEmpty -> PNode(p, r, pqt1, pqt2, pqt3, new_node (point) (get_rect_squ r 4))
            | 4 -> PNode(p, r, pqt1, pqt2, pqt3, insert pqt4 point)
            | _ -> failwith "insert: Not possible"

    (* Draws a pqt on screen. *)
    let draw_pqt = fun pqt ->
      open_graph "";
      let rec aux = fun pqt ->
        match pqt with
          | PEmpty -> ();
          | PNode(p, r, pqt1, pqt2, pqt3, pqt4) ->
            draw_my_rect (get_rect_squ r 1);
            draw_my_rect (get_rect_squ r 2);
            draw_my_rect (get_rect_squ r 3);
            draw_my_rect (get_rect_squ r 4);
            aux pqt1;
            aux pqt2;
            aux pqt3;
            aux pqt4
      in aux pqt

  end
;;
