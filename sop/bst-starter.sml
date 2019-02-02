(* Binary search trees on integers *)
(* Uses the bintree datatype *)

use "bintree-soln.sml"; 

fun singleton v = Node(Leaf, v, Leaf)

fun insert x Leaf = Leaf (* replace this stub *)
  | insert x (t as (Node(l,v,r))) =
    Leaf (* replace this stub *)

fun listToTree xs = (* Hint: use foldl *)
    Leaf (* replace this stub *)

fun member x Leaf = false
  | member x (Node(l,v,r)) = false (* replace this stub *)
    
(* Test cases *)								     

(* val test_bst = listToTree [4,2,3,6,1,7,5]; *) 

(* val test_member = map (fn i => (i, member i test_bst)) [0,1,2,3,4,5,6,7,8] *)
