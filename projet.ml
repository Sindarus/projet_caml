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
   p1, p2, p3 et p4 ne sont peut être pas initialisés.
   Renvoie 0 si le point n'appartient pas au quadtree. C'est a dire 1) si le
   quadtree passé en premier lieu est PEmpty, ou 2) si le point est hors de sa
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

(* get rect formed if you take the cadr'th cadr of r *)
let get_rect_cadr = fun r cadr ->
    let c = get_centre r in
    match cadr with
        | 1 -> {top = r.top ; left = r.left ; right = c.x ; bottom = c.y}
        | 2 -> {top = r.top ; right = r.right ; left = c.x ; bottom = c.y}
        | 3 -> {bottom = r.bottom ; left = r.left ; top = c.y ; right = c.x}
        | 4 -> {bottom = r.bottom ; right = r.right ; top = c.y ; left = c.x}
        | _ -> failwith "get_rect_cadr : valeur non autorisée"
;;

(* Return an empty node that has point p and support rect*)
let new_node = fun p r ->
    PNoeud(p, r, PEmpty, PEmpty, PEmpty, PEmpty)
;;

let rec insere = fun tree point ->
    match tree with
        | PEmpty -> failwith "insere : tree est vide"
        | PNoeud(p, r, p1, p2, p3, p4) when p = point ->
            failwith "insere : point est déja dans tree"
        | PNoeud(p, r, p1, p2, p3, p4) ->
            let cadr_num = (get_cadr_num_pt tree point) in match cadr_num with
                | 0 -> failwith "insere : point pas couvert par tree"
                | 1 when p1 = PEmpty -> PNoeud(p, r, new_node (point) (get_rect_cadr tree 1), p2, p3, p4)
                | 1 -> PNoeud(p, r, insere p1 point, p2, p3, p4)
                | 2 when p2 = PEmpty -> PNoeud(p, r, p1, new_node point (get_rect_cadr tree 2), p3, p4)
                | 2 -> PNoeud(p, r, p1, insere p2 point, p3, p4)
                | 3 when p3 = PEmpty -> PNoeud(p, r, p1, p2, new_node point (get_rect_cadr tree 3), p4)
                | 3 -> PNoeud(p, r, p1, p2, insere p3 point, p4)
                | 2 when p4 = PEmpty -> PNoeud(p, r, p1, p2, p3, new_node point (get_rect_cadr tree 4))
                | 4 -> PNoeud(p, r, p1, p2, p3, insere p4 point)
                | _ -> failwith "insere : Pas possible"
;;

(* tests *)
let p = new_pquadtree 100;;
let centre = let PNoeud(x, r, p1, p2, p3, p4) = p in (get_centre r);;
