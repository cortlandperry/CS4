(* midterm.mli: interface file for the CS 4 midterm exam, 2017 *)

(* 2.1 *)

val group3 : 'a list -> ('a * 'a * 'a) list
val avg3 : int list -> int list


(* 2.2 *)

val take : int -> 'a list -> 'a list * 'a list


val take2 : int -> 'a list -> 'a list * 'a list * 'a list

val mergeN : int -> 'a list -> 'a list


val merge_sort : 'a list -> 'a list


(* 3.1 *)

val filteri : (int -> bool) -> 'a list -> 'a list
val filteriv : (int -> 'a -> bool) -> 'a list -> 'a list

(* 3.2 *)

val bests_so_far : ('a -> 'a -> bool) -> 'a list -> 'a list


(* 3.3 *)

val contfrac : (int -> float) -> (int -> float) -> int -> float
val golden_ratio_terms : int -> float
val pi_terms : int -> float
val e_terms : int -> float
val converge : (int -> float) -> float -> float


(* Section 4. *)

type tree = Leaf | Node of int * int * tree * tree
val depth : tree -> int
val data : tree -> int
val make_node : int -> tree -> tree -> tree


val min_avl_tree : tree -> int option
val max_avl_tree : tree -> int option
val is_avl_tree : tree -> bool


(* 4.1 *)

val search : int -> tree -> bool


(* 4.2 *)

val left_rotate : tree -> tree
val right_rotate : tree -> tree


(* 4.3 *)

val insert : int -> tree -> tree

