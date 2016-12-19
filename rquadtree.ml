#load "graphics.cma";;
open Graphics;;

#use "point_rect.ml";;
open Point_rect;;

module Rqt =
  struct
    type colour =
      | White
      | Black

    type rquadtree =
      | Uni of colour
      | RQ of rquadtree * rquadtree * rquadtree * rquadtree

    let draw_rqt rqt support_size =
      open_graph "";
      let rec aux rqt support_rect =
        match rqt with
          | Uni(c) ->
            (if c = White then set_color white else set_color black);
            fill_my_rect support_rect
          | RQ(rqt1, rqt2, rqt3, rqt4) ->
            aux rqt1 (get_rect_squ support_rect 1);
            aux rqt2 (get_rect_squ support_rect 2);
            aux rqt3 (get_rect_squ support_rect 3);
            aux rqt4 (get_rect_squ support_rect 4);
            set_color black;
            draw_my_rect (get_rect_squ support_rect 1);
            draw_my_rect (get_rect_squ support_rect 2);
            draw_my_rect (get_rect_squ support_rect 3);
            draw_my_rect (get_rect_squ support_rect 4);
      in
      aux rqt (new_rect support_size 0 0 support_size)

  end
;;

open Rqt;;
let test_draw_rqt () =
  let a = RQ(
    Uni Black,
    Uni White,
    Uni White,
    RQ(Uni White, Uni Black, Uni White, Uni Black)
  ) in
  draw_rqt a 512
;;