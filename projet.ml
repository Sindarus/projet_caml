module Pqt =
  struct
    type rect = {top : int; bottom : int; right : int; left : int}

    type point = {x : int; y : int}

    type pquadtree =
      | PEmpty
      | PNoeud of point * rect * pquadtree * pquadtree * pquadtree * pquadtree

    (* Returns a pquadtree that has n*n for support rectangle *)
    let new_pquadtree = fun n ->
      PNoeud({x=0; y=0},
           {left=0; bottom=0; top=n; right=n},
           PEmpty, PEmpty, PEmpty, PEmpty)

    (* Takes a 'rect' and returns the central point of that rectangle *)
    let get_center = fun r ->
      {x = (r.right - r.left) / 2 ; y = (r.top - r.bottom) / 2}

    (* Returns true if point is outside the rect rectangle *)
    let out_rect = fun rect point ->
      point.x > rect.right
      || point.x < rect.left
      || point.y > rect.top
      || point.y < rect.bottom

    (* From a pquadtree and a point, this function returns the number of the
       square in which the point is.
       This does not mean that the pquadtree has been splitted yet :
       p1, p2, p3 et p4 can be uninitialized.
       This function returns 0 if the point cannot be contained by the tree.
       That means 1) when tree is PEmpty 2) when point is outside of tree's
       support rectangle *)
    let get_squ_num_pt = fun tree point ->
      match tree with
        | PEmpty -> 0
        | PNoeud(p, r, p1, p2, p3, p4) when (out_rect r point) -> 0
        | PNoeud(p, r, p1, p2, p3, p4) -> let center = (get_center r) in
          match point.x with
            | x when x < center.x -> ( match point.y with
              | y when y < center.y -> 3
              | y           -> 1 )
            | x           -> ( match point.y with
              | y when y < center.y -> 4
              | y           -> 2 )

    (* Returns true if the point is a value of tree.
       Careful : this is different from knowing weather the point is supported
       by the tree's support rectangle *)
    let rec pbelong = fun tree point ->
      match tree with
        | PEmpty -> false
        | PNoeud(p, r, p1, p2, p3, p4) when p = point -> true
        | PNoeud(p, r, p1, p2, p3, p4) ->
          let cadr_num = get_squ_num_pt tree point in match cadr_num with
            | 0 -> false
            | 1 -> pbelong p1 point
            | 2 -> pbelong p2 point
            | 3 -> pbelong p3 point
            | 4 -> pbelong p4 point
            | _ -> failwith "pbelong: Not possible"

    (* Returns the list of squares you have to go through in order to reach
       point, given that point is a value of tree. *)
    let rec ppath = fun tree point ->
      match tree with
        | PEmpty -> failwith "ppath: point is not in tree"
        | PNoeud(p, r, p1, p2, p3, p4) when p = point -> []
        | PNoeud(p, r, p1, p2, p3, p4) ->
          let cadr_num = get_squ_num_pt tree point in match cadr_num with
            | 0 -> failwith "ppath: point is not in tree 2"
            | 1 -> "NO" :: (ppath p1 point)
            | 2 -> "NE" :: (ppath p2 point)
            | 3 -> "SO" :: (ppath p3 point)
            | 4 -> "NE" :: (ppath p4 point)
            | _ -> failwith "ppath: not possible"

    (* Get the squ'th square of rect *)
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
      PNoeud(p, r, PEmpty, PEmpty, PEmpty, PEmpty)

    (* Given a tree that is not PEmpty and a point, this function splits the deepest square
       in tree whose support rectangle supports point, by adding a pquadtree there,
       that has point for value *)
    let rec insert = fun tree point ->
      match tree with
        | PEmpty -> failwith "insert: tree is empty"
        | PNoeud(p, r, p1, p2, p3, p4) when p = point ->
          failwith "insert: point is already in tree"
        | PNoeud(p, r, p1, p2, p3, p4) ->
          let cadr_num = (get_squ_num_pt tree point) in match cadr_num with
            | 0 -> failwith "insert: point is not supported by tree's support rectangle"
            | 1 when p1 = PEmpty -> PNoeud(p, r, new_node (point) (get_rect_squ r 1), p2, p3, p4)
            | 1 -> PNoeud(p, r, insert p1 point, p2, p3, p4)
            | 2 when p2 = PEmpty -> PNoeud(p, r, p1, new_node (point) (get_rect_squ r 2), p3, p4)
            | 2 -> PNoeud(p, r, p1, insert p2 point, p3, p4)
            | 3 when p3 = PEmpty -> PNoeud(p, r, p1, p2, new_node (point) (get_rect_squ r 3), p4)
            | 3 -> PNoeud(p, r, p1, p2, insert p3 point, p4)
            | 4 when p4 = PEmpty -> PNoeud(p, r, p1, p2, p3, new_node (point) (get_rect_squ r 4))
            | 4 -> PNoeud(p, r, p1, p2, p3, insert p4 point)
            | _ -> failwith "insert: Not possible"
  end;;

(* tests *)
(*
let p = Pqt.new_pquadtree 100;;
let center = let Pqt.PNoeud(x, r, p1, p2, p3, p4) = p in (Pqt.get_center r);;
*)
