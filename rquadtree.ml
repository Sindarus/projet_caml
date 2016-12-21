#load "graphics.cma";;
open Graphics;;

#use "point_rect.ml";;
open PointRect;;

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

    let rec inverse_rqt rqt =
      match rqt with
        | Uni(c) ->
          if c = White
          then Uni(Black) else Uni(White)
        | RQ(rqt1, rqt2, rqt3, rqt4) ->
          RQ(inverse_rqt rqt1,
             inverse_rqt rqt2,
             inverse_rqt rqt3,
             inverse_rqt rqt4)

    let rec inter_rqt rqta rqtb =
      match (rqta, rqtb) with
        | (Uni(White), _) -> Uni(White)
        | (_, Uni(White)) -> Uni(White)
        (* From here on, we know rqta and rqtb are not White *)
        | (Uni(Black), Uni(Black)) ->
          Uni(Black)
        | (Uni(Black), RQ(rqt1, rqt2, rqt3, rqt4)) ->
          let c = RQ(inter_rqt (Uni(Black)) rqt1,
                     inter_rqt (Uni(Black)) rqt2,
                     inter_rqt (Uni(Black)) rqt3,
                     inter_rqt (Uni(Black)) rqt4)
          in if c = RQ(Uni White, Uni White, Uni White, Uni White)
          (* This should only be true if rqtb is not normalized *)
          then Uni White else c
        | (RQ(rqt1, rqt2, rqt3, rqt4), Uni(Black)) ->
          let c = RQ(inter_rqt (Uni(Black)) rqt1,
                     inter_rqt (Uni(Black)) rqt2,
                     inter_rqt (Uni(Black)) rqt3,
                     inter_rqt (Uni(Black)) rqt4)
          in if c = RQ(Uni White, Uni White, Uni White, Uni White)
          then Uni White else c
        | (RQ(rqta1, rqta2, rqta3, rqta4), RQ(rqtb1, rqtb2, rqtb3, rqtb4)) ->
          let c = RQ(inter_rqt rqta1 rqtb1,
                     inter_rqt rqta2 rqtb2,
                     inter_rqt rqta3 rqtb3,
                     inter_rqt rqta4 rqtb4)
          in if c = RQ(Uni White, Uni White, Uni White, Uni White)
          then Uni White else c

    let rec union_rqt rqta rqtb =
      inverse_rqt (inter_rqt (inverse_rqt rqta) (inverse_rqt rqtb))

    let rec rearrange_rqt rqt permut_func =
      match rqt with
        | Uni(c) -> Uni(c)
        | RQ(rqt1, rqt2, rqt3, rqt4) ->
          let (a, b, c, d) = permut_func (rqt1, rqt2, rqt3, rqt4)
          in RQ(rearrange_rqt a permut_func,
                rearrange_rqt b permut_func,
                rearrange_rqt c permut_func,
                rearrange_rqt d permut_func)

    let vert_sym_rqt rqt =
      let permut rqts =
        let(a, b, c, d) = rqts in (b, a, d, c)
      in
      rearrange_rqt rqt permut

    let horiz_sym_rqt rqt =
      let permut rqts =
        let(a, b, c, d) = rqts in (c, d, a, b)
      in
      rearrange_rqt rqt permut

  end
;;
