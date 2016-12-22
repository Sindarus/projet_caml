#load "graphics.cma";;
open Graphics;;

module PointRect =
  struct
    type rect = {top : int; bottom : int; right : int; left : int}
    type point = {x : int; y : int}

    (* Syntaxic sugar *)
    let new_point = fun px py ->
      {x=px; y=py}

    (* Syntaxic sugar *)
    let new_rect = fun rtop rbottom rleft rright ->
      {top=rtop; bottom=rbottom; right=rright; left=rleft}

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

    (* Converts the rect format to (x, y, w, h) where x y are the coordinates
       of the lower left point and w and h are the width and height of the rect *)
    let rect_to_xywh = fun rect ->
      let {top=rtop; bottom=rbottom; left=rleft; right=rright} = rect in
      (rleft, rbottom, rright-rleft, rtop-rbottom);;

    (* Draws a rect on screen. Graph must be opened already. *)
    let draw_my_rect = fun r ->
      let (x, y, w, h) = rect_to_xywh r in
      draw_rect x y w h

    let fill_my_rect = fun r ->
      let (x, y, w, h) = rect_to_xywh r in
      fill_rect x y w h

  end
;;