
(*Zadanie 1 - punkt*)
module Point_3D_module = struct
  type point3D = { x: int; y: int; z: int }

  let getDistance a b =
    sqrt ((float_of_int (a.x - b.x))**2.0 +. (float_of_int (a.y - b.y))**2.0 +. (float_of_int (a.z - b.z))**2.0)

  let createPoint x y z = { x = x; y = y; z = z }
end
module type Point_3D_module = sig
  type point3D
  val getDistance : point3D -> point3D -> float
  val createPoint : int -> int -> int -> point3D
end

(* Zadanie 2 - odcinek*)
module Line = struct
  type line = { a: Point_3D_module.point3D; b: Point_3D_module.point3D }

  let create_line a b = { a = a; b = b }

  let get_length line = Point_3D_module.getDistance line.a line.b
end
module type Line = sig
  type line
  val create_line : Point_3D_module.point3D -> Point_3D_module.point3D -> line
  val get_length : line -> float
end

(*Zadanie 3 - drzewo binarne*)
module Binary_tree = struct
  type tree =
    | Empty
    | Node of int * tree * tree   (*value, left child, right child*)

  let rec add_node value = function (* = function is shorter way of writing match argument with*)
    | Empty -> Node (value, Empty, Empty)
    | Node (v, left, right) ->
        if value < v then Node (v, add_node value left, right) (* call adding to left subtree *)
        else if value > v then Node (v, left, add_node value right) (* or to right subtree*)
        else Node (v, left, right)  (* ignore duplicates *)

let rec delete_node value = function
  | Empty -> Empty
  | Node (v, left, right) -> (* search for the node to be deleted *)
      if value < v then (* if smaller, go to the left subtree *)
        Node (v, delete_node value left, right)
      else if value > v then (* if bigger, go to the right subtree *)
        Node (v, left, delete_node value right)
      else
        match left, right with
        | Empty, _ -> right  (* No left child, replace deleted node with the right one *)
        | _, Empty -> left   (* No right child, replace deleted node with the left one*)
        | _, _ ->
            let smallestBigger = find_min right in  (* Node to be deleted has both left and right children *)
            Node (smallestBigger, left, delete_node smallestBigger right) 
            (* Replace deleted node with the smallest node from the right subtree, and delete that smallest node *)

  and find_min = function
    | Empty -> failwith "Empty tree"
    | Node (v, Empty, _) -> v (* if there are no smaller values, return current value*)
    | Node (_, left, _) -> find_min left (* keep going to the left subtree, while we can *)

    let rec inorder_walk = function (* = function is shorter way of writing match argument with*)
    | Empty -> []
    | Node (v, left, right) -> inorder_walk left @ [v] @ inorder_walk right

  let rec preorder_walk = function
    | Empty -> []
    | Node (v, left, right) -> [v] @ preorder_walk left @ preorder_walk right

  let rec postorder_walk = function
    | Empty -> []
    | Node (v, left, right) -> postorder_walk left @ postorder_walk right @ [v]
end

  (*test tasks 1,2 and 3*)
  let point1 = Point_3D_module.createPoint 1 2 3
  let point2 = Point_3D_module.createPoint 4 5 6

  let line = Line.create_line point1 point2 

  let length = Line.get_length line 
  let _ = print_endline "Length of the line:"
  let _ = print_endline (string_of_float length)

 
  let tree = Binary_tree.Node(1, Binary_tree.Node(2, Binary_tree.Empty, Binary_tree.Empty), Binary_tree.Node(3, Binary_tree.Empty, Binary_tree.Empty))
  
  let tree_elements = Binary_tree.preorder_walk tree
  let _ = print_endline "Contents of the tree:"
  let _ = print_endline (String.concat " " (List.map string_of_int tree_elements))


  (*Zadanie 4:*)
module type AbstractCordinate = sig
  type 'a cordinate
  val create : 'a -> 'a cordinate
end

module AbstractCordinate = struct
  type 'a cordinate = 'a
  let create x = x
end

 (*Funktor to "funkcja" która bierze inny moduł jako parametr i "zwraca" moduł*)
module MakePoint (Cordinate1 : AbstractCordinate)( Cordinate2 : AbstractCordinate) = struct
  type 'a point = { x: 'a Cordinate1.cordinate; y: 'a Cordinate2.cordinate }
end
module AbstractPoint = MakePoint (AbstractCordinate) (AbstractCordinate)

let cordinate1 = AbstractCordinate.create 2
let cordinate2 = AbstractCordinate.create 4
let point1 = AbstractPoint.{ x = cordinate1; y = cordinate2 }
let _ = print_endline (string_of_int point1.x)
let _ = print_endline (string_of_int point1.y)

(*Zadanie 5 i 6*)
module Translation = struct
  type  translation = { x: int; y: int; z: int }
  let create x y z = { x = x; y = y; z = z }
end
module type Translation = sig
  type translation
  val create : int -> int -> int -> translation
end

module Translate_Point (P : Point_3D_module) (T : Translation) = struct
  let translate point translation =
    P.createPoint (point.Point_3D_module.x + translation.Translation.x)
                  (point.Point_3D_module.y + translation.Translation.y)
                  (point.Point_3D_module.z + translation.Translation.z)
end
module TranslatedPoint = Translate_Point (Point_3D_module) (Translation)

module Translate_Segment (L : Line) (T : Translation) = struct
  let translate line translation =
    L.create_line (TranslatedPoint.translate line.Line.a translation)
                  (TranslatedPoint.translate line.Line.b translation)
end
module TranslatedLIne = Translate_Segment (Line) (Translation)

let point = Point_3D_module.createPoint 1 2 3
let translation = Translation.create 1 1 1
let point = TranslatedPoint.translate point translation
let _ = print_endline (string_of_int point.x)

let line = Line.create_line (Point_3D_module.createPoint 1 1 1) (Point_3D_module.createPoint 2 2 2)
let line = TranslatedLIne.translate line translation
let _ = print_endline (string_of_float (Line.get_length line))

