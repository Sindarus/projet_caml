#load "graphics.cma";;
open Graphics;;

module Pqt =
  struct
    type rect = {top : int; bottom : int; right : int; left : int}

    type point = {x : int; y : int}

    type pquadtree =
      | PEmpty
      | PNode of point * rect * pquadtree * pquadtree * pquadtree * pquadtree

    (* Syntaxic sugar *)
    let new_point = fun px py ->
      {x=px; y=py}

    (* Syntaxic sugar *)
    let new_rect = fun rtop rbottom rleft rright ->
      {top=rtop; bottom=rbottom; right=rright; left=rleft}

    (* Returns a pquadtree that has n*n for support rectangle *)
    let new_pquadtree = fun n ->
      PNode(new_point 0 0,
            new_rect n 0 0 n,
            PEmpty, PEmpty, PEmpty, PEmpty)

    (* Takes a 'rect' and returns the central point of that rectangle *)
    let get_center = fun r ->
      new_point
        ((r.right - r.left) / 2 + r.left)
        ((r.top - r.bottom) / 2 + r.bottom)

    (* Returns true if point is outside the rect rectangle *)
    let out_rect = fun rect point ->
      point.x > rect.right
      || point.x < rect.left
      || point.y > rect.top
      || point.y < rect.bottom

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
        | PNode(p, r, pqt1, pqt2, pqt3, pqt4) -> let center = (get_center r) in
          match point.x with
            | x when x < center.x -> ( match point.y with
              | y when y < center.y -> 3
              | y           -> 1 )
            | x                   -> ( match point.y with
              | y when y < center.y -> 4
              | y           -> 2 )

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

    (* Get (as a rect) the squ'th square of rect, where squ is a number
       between 1 and 4 *)
    let get_rect_squ = fun rect squ ->
      let c = get_center rect in
      match squ with
        | 1 -> {top = rect.top ; left = rect.left ; right = c.x ; bottom = c.y}
        | 2 -> {top = rect.top ; right = rect.right ; left = c.x ; bottom = c.y}
        | 3 -> {bottom = rect.bottom ; left = rect.left ; top = c.y ; right = c.x}
        | 4 -> {bottom = rect.bottom ; right = rect.right ; top = c.y ; left = c.x}
        | _ -> failwith "get_rect_squ: the value you provided for squ is unauthorized"

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

    (* Converts the rect format to (x, y, w, h) where x y are the coordinates
       of the lower left point and w and h are the width and height of the rect *)
    let rect_to_xywh = fun rect ->
      let {top=rtop; bottom=rbottom; left=rleft; right=rright} = rect in
      (rleft, rbottom, rright-rleft, rtop-rbottom);;

    (* Draws a rect on screen. Graph must be opened already. *)
    let draw_my_rect = fun r ->
      let (x, y, w, h) = rect_to_xywh r in
      draw_rect x y w h

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

  end;;

(* tests *)
(*
let p = Pqt.new_pquadtree 100;;
let center = let Pqt.PNode(x, r, pqt1, pqt2, pqt3, pqt4) = p in (Pqt.get_center r);;
*)
