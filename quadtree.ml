#load "graphics.cma";;
open Graphics;;

#use "point_rect.ml";;
open PointRect;;

module Qtree =
  struct
    type quadtree =
      | Empty
      | Q of rect * (rect list) * (rect list) *
             quadtree * quadtree * quadtree * quadtree;;
    
    let draw_qtree qtree =
      open_graph "";
      let rec aux = fun qtree ->
        match qtree with
          | Empty -> ();
          | Q(r, lv, lh, q1, q2, q3, q4) ->
            List.map draw_my_rect (lv@lh);
            draw_my_rect (get_rect_squ r 1);
            draw_my_rect (get_rect_squ r 2);
            draw_my_rect (get_rect_squ r 3);
            draw_my_rect (get_rect_squ r 4);
            aux q1;
            aux q2;
            aux q3;
            aux q4
      in aux qtree

    (* Returns 0 if rect does not crosses sup_rect's mediane, 1 if it crosses
       its vertical mediane and 2 if it crosses its horizontal mediane *)
    let rect_is_in_lvlh sup_rect rect =
      let c = get_center sup_rect in
      if (rect.left <= c.x) && (c.x <= rect.right) then 1
      else if (rect.bottom <= c.y) && (c.y <= rect.top) then 2
      else 0

    let support_contains_rect sup_rect rect =
      (rect.top <= sup_rect.top) &&
      (rect.bottom >= sup_rect.bottom) &&
      (rect.left >= sup_rect.left) &&
      (rect.right <= sup_rect.right)

    (* Returns the id number of the square of sup_rect in which rect can be
       fully contained. The returned id is between 1 and 4 for NO, NE, SO, SE.
       If rect crosses one of the mediane, this returns 0.
       sup_rect has to contain rect. *)
    let what_square_contains_rect sup_rect rect =
      let c = get_center sup_rect in
      if (rect.bottom > c.y) then (
        if (rect.right < c.x) then 1
        else if (rect.left > c.x) then 2
        else 0
      )
      else if (rect.top < c.y) then (
        if (rect.right < c.x) then 3
        else if (rect.left > c.x) then 4
        else 0
      )
      else 0

    let new_qtree sup_rect =
      Q(sup_rect, [], [], Empty, Empty, Empty, Empty)

    let insert_rect qtree rect =
      (
        match qtree with
          | Empty -> failwith "insert_rect: qtree is empty. Cannot insert rect in empty tree."
          | Q(r, lv, lh, q1, q2, q3, q4) ->
              if not (support_contains_rect r rect) then failwith "qtree cannot contain rect"
      );
      let rec aux qtree rect sup_rect_qtree =
        match qtree with
          | Empty -> aux (new_qtree sup_rect_qtree) rect sup_rect_qtree
          | Q(r, lv, lh, q1, q2, q3, q4) -> 
            match rect_is_in_lvlh r rect with
              | 1 -> Q(r, lv@[rect], lh, q1, q2, q3, q4)
              | 2 -> Q(r, lv, lh@[rect], q1, q2, q3, q4)
              | _ -> match what_square_contains_rect r rect with
                | 1 -> Q(r, lv, lh, aux q1 rect (get_rect_squ r 1), q2, q3, q4)
                | 2 -> Q(r, lv, lh, q1, aux q2 rect (get_rect_squ r 2), q3, q4)
                | 3 -> Q(r, lv, lh, q1, q2, aux q3 rect (get_rect_squ r 3), q4)
                | 4 -> Q(r, lv, lh, q1, q2, q3, aux q4 rect (get_rect_squ r 4))
                | _ -> failwith "insert_rect : not supposed to happen"
      in match qtree with
        | Empty -> failwith "insert_rect: this should never happen."
        | Q(r, _, _, _, _, _, _) -> aux qtree rect r

  end
;;