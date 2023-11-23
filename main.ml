

module Point3DModule = struct
  type point3D = { x: int; y: int; z: int }

  let getDistance a b =
    sqrt ((float_of_int (a.x - b.x))**2.0 +. (float_of_int (a.y - b.y))**2.0 +. (float_of_int (a.z - b.z))**2.0)

  let createPoint x y z = { x = x; y = y; z = z }
end

module Line = struct
  type line = { a: Point3DModule.point3D; b: Point3DModule.point3D }

  let createLine a b = { a = a; b = b }

  let getLength line = Point3DModule.getDistance line.a line.b
end

module BinaryTree = struct
  type tree =
    | Empty
    | Node of int * tree * tree   (*value, left child, right child*)

  let rec add_node value = function
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

 (*TODO: walk the tree*)
end