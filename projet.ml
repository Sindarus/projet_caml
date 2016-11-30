type rect = {top : int; bottom : int; right : int; left : int};;

type point = {x : int; y : int};;

type pquadtree =
PEmpty | PNoeud of point * rect * pquadtree * pquadtree * pquadtree * pquadtree;;


(* Prend un rect et renvoie le point central du rect*)
let get_centre = fun r ->
    {x = (r.right - r.left) / 2 ; y = (r.top - r.bottom) / 2}
;;

(* Renvoie un pquadtree de couverture n*n *)
let new_pquadtree = fun n ->
    PNoeud({x=0;y=0}, {left=0; bottom=0; top=n; right=n}, PEmpty, PEmpty, PEmpty, PEmpty)
;;

(* Renvoie true si point est hors de la couverture du rect r *)
let hors_rect = fun r point ->
    point.x > r.right || point.x < r.left || point.y > r.top || point.y < r.bottom
;;

(* Prend un pquadtree et un point, renvoie le numéro du cadran de ce pquadtree
   dans lequel se trouve le point.
   Attention, cela ne signifie pas que le pquadtree a déja été partitionné :
   p1, p2, p3 et p4 sont peut être pas initialisé.
   Renvoie 0 si le point n'appartient pas au quadtree. C'est a dire 1) si le
   quadtree passé en premier lieu est PEmpty, ou si le point est hors de sa
   couverture. *)
let get_cadr_num_pt = fun tree point ->
    match tree with
        | PEmpty -> 0
        | PNoeud(p, r, p1, p2, p3, p4) when (hors_rect r point) -> 0
        | PNoeud(p, r, p1, p2, p3, p4) -> let centre = (get_centre r) in
            match point.x with
                | x when x < centre.x -> ( match point.y with
                    | y when y < centre.y -> 3
                    | y                   -> 1 )
                | x                   -> ( match point.y with
                    | y when y < centre.y -> 4
                    | y                   -> 2 )
;;

(* Renvoie true si le point est une valeur dans tree.
   Attention, c'est une notion différente de « si le point est couvert par
   l'arbre » *)
let rec pappartient = fun tree point ->
    match tree with
        | PEmpty -> false
        | PNoeud(p, r, p1, p2, p3, p4) when p = point -> true
        | PNoeud(p, r, p1, p2, p3, p4) ->
            let cadr_num = get_cadr_num_pt tree point in match cadr_num with
                | 0 -> false
                | 1 -> pappartient p1 point
                | 2 -> pappartient p2 point
                | 3 -> pappartient p3 point
                | 4 -> pappartient p4 point
                | _ -> failwith "Pas possible"
;;

(* Renvoie une liste des cadrans qu'il faut parcourir pour aboutir a point.
   Il faut que point soit dans tree. *)
let rec pchemin = fun tree point ->
    match tree with
        | PEmpty -> failwith "point n'est pas dans tree"
        | PNoeud(p, r, p1, p2, p3, p4) when p = point -> []
        | PNoeud(p, r, p1, p2, p3, p4) ->
            let cadr_num = get_cadr_num_pt tree point in match cadr_num with
                | 0 -> failwith "point n'est pas dans tree 2"
                | 1 -> "NO" :: (pchemin p1 point)
                | 2 -> "NE" :: (pchemin p2 point)
                | 3 -> "SO" :: (pchemin p3 point)
                | 4 -> "NE" :: (pchemin p4 point)
                | _ -> failwith "Pas possible2"
;;

(* tests *)
let p = new_pquadtree 100;;
let centre = let PNoeud(x, r, p1, p2, p3, p4) = p in (get_centre r);;
