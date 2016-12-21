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
      | Plain of colour
      | RQ of rquadtree * rquadtree * rquadtree * rquadtree

    (* Draws a rqt on screen. *)
    let draw_rqt rqt support_size =
      open_graph "";
      let rec aux rqt support_rect =
        match rqt with
          | Plain(c) ->
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

    (* Returns the inverse of the rqt in argument. Inverse means that
       black becomes white and vice versa. *)
    let rec inverse_rqt rqt =
      match rqt with
        | Plain(c) ->
          if c = White
          then Plain(Black) else Plain(White)
        | RQ(rqt1, rqt2, rqt3, rqt4) ->
          RQ(inverse_rqt rqt1,
             inverse_rqt rqt2,
             inverse_rqt rqt3,
             inverse_rqt rqt4)

    (* Returns the intersection of rqta and rqtb. The intersection of rqta and
       rqtb is a rqtc such as rqtc is black only and only where rqta AND rqtb
       are black. *)
    let rec inter_rqt rqta rqtb =
      match (rqta, rqtb) with
        | (Plain(White), _) -> Plain(White)
        | (_, Plain(White)) -> Plain(White)
        (* From here on, we know rqta and rqtb are not White *)
        | (Plain(Black), Plain(Black)) ->
          Plain(Black)
        | (Plain(Black), RQ(rqt1, rqt2, rqt3, rqt4)) ->
          let c = RQ(inter_rqt (Plain(Black)) rqt1,
                     inter_rqt (Plain(Black)) rqt2,
                     inter_rqt (Plain(Black)) rqt3,
                     inter_rqt (Plain(Black)) rqt4)
          in if c = RQ(Plain White, Plain White, Plain White, Plain White)
          (* This should only be true if rqtb is not normalized *)
          then Plain White else c
        | (RQ(rqt1, rqt2, rqt3, rqt4), Plain(Black)) ->
          let c = RQ(inter_rqt (Plain(Black)) rqt1,
                     inter_rqt (Plain(Black)) rqt2,
                     inter_rqt (Plain(Black)) rqt3,
                     inter_rqt (Plain(Black)) rqt4)
          in if c = RQ(Plain White, Plain White, Plain White, Plain White)
          then Plain White else c
        | (RQ(rqta1, rqta2, rqta3, rqta4), RQ(rqtb1, rqtb2, rqtb3, rqtb4)) ->
          let c = RQ(inter_rqt rqta1 rqtb1,
                     inter_rqt rqta2 rqtb2,
                     inter_rqt rqta3 rqtb3,
                     inter_rqt rqta4 rqtb4)
          in if c = RQ(Plain White, Plain White, Plain White, Plain White)
          then Plain White else c

    (* Returns the rqt union of rqta and rqtb *)
    let rec union_rqt rqta rqtb =
      inverse_rqt (inter_rqt (inverse_rqt rqta) (inverse_rqt rqtb))

    (* Returns the result of applying the permut_func permutation to rqt and
       its children *)
    let rec rearrange_rqt rqt permut_func =
      match rqt with
        | Plain(c) -> Plain(c)
        | RQ(rqt1, rqt2, rqt3, rqt4) ->
          let (a, b, c, d) = permut_func (rqt1, rqt2, rqt3, rqt4)
          in RQ(rearrange_rqt a permut_func,
                rearrange_rqt b permut_func,
                rearrange_rqt c permut_func,
                rearrange_rqt d permut_func)

    (* Returns the vertical symetry of rqt *)
    let vert_sym_rqt rqt =
      let permut rqts =
        let(a, b, c, d) = rqts in (b, a, d, c)
      in
      rearrange_rqt rqt permut

    (* Returns the horizontal symetry of rqt *)
    let horiz_sym_rqt rqt =
      let permut rqts =
        let(a, b, c, d) = rqts in (c, d, a, b)
      in
      rearrange_rqt rqt permut

    (* Returns a string representing the rqt *)
    let rec code_rqt rqt =
      match rqt with
        | Plain(c) ->
          if c = Black then "11" else "10"
        | RQ(rqt1, rqt2, rqt3, rqt4) ->
          String.concat "" [ "0";
            code_rqt rqt1;
            code_rqt rqt2;
            code_rqt rqt3;
            code_rqt rqt4
          ]

    (* Returns str without the first n characters, where n is an int*)
    let str_wo_first_n str n =
      String.sub str n (String.length str - n)

    (* Returns the rqt from a string representation *)
    let uncode_rqt str =
      let rec aux str =
        match str.[0] with
          | '1' ->
            if str.[1] = '0'
            then (Plain White, str_wo_first_n str 2)
            else (Plain Black,  str_wo_first_n str 2)
          | '0' ->
            let r0 = str_wo_first_n str 1 in
            let (rqt1, r1) = aux r0 in
            let (rqt2, r2) = aux r1 in
            let (rqt3, r3) = aux r2 in
            let (rqt4, r4) = aux r3 in
            (RQ(rqt1, rqt2, rqt3, rqt4), r4)
          | _ -> failwith "uncode_rqt: string not valid rqt."
      in let (rqt, r) = aux str
      in rqt

  end
;;
