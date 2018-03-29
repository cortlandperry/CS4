(* A.1.a *)

type mobile = Mobile of branch * branch
and branch = 
	| Weight of int * int
	| Structure of int * mobile
	
let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

let left_branch = function
	| Mobile (l, _) -> l
	
let right_branch = function
	| Mobile (_, r) -> r

let branch_length b = 
	match b with
		| Weight (l, w) -> l
		| Structure (l, m) -> l
		
let branch_structure b =
	match b with
		| Weight (l, w) -> `Weight w
		| Structure (l, m) -> `Structure m
	
(* A.1.b *)
let rec branch_weight1 b =
	match b with 
		| Weight (l, w) -> w
		| Structure (l, m) -> total_weight1 m
and total_weight1 (Mobile (l, r)) = 
	(branch_weight1 l + branch_weight1 r)

let rec branch_weight2 b = 
	match (branch_structure b) with
		| `Weight w -> w
		| `Structure m -> total_weight2 m
and total_weight2 m =
	(branch_weight2 (left_branch m) + branch_weight2 (right_branch m))
	
(* A.1.c *)
let rec is_balanced m = 
	let iter b = 
		match b with
			| Weight (l, w) -> true
			| Structure (l, m) -> is_balanced m
	in iter(left_branch m) && iter(right_branch m) && 
		((branch_length (left_branch m)) * branch_weight2 (left_branch m)
		= (branch_length (right_branch m)) * branch_weight2 (right_branch m))

	
	
(* A.1.d *)
type mobile' = { left: branch'; right: branch' }
and branch' = Branch' of int * contents
and contents = Weight' of int | Structure' of mobile'

let make_mobile' l r = { left = l; right = r }
let make_weight' l w = Branch' (l, (Weight' w))
let make_structure' l m = Branch' (l, (Structure' m))

let left_branch' {left; right} = left
let right_branch' {left; right} = right
let branch_length' (Branch' (l, c)) = l

let branch_structure' (Branch' (l, c)) = 
	match c with
	| Weight' w -> `Weight w
	| Structure' c -> `Structure c
		

let rec branch_weight' b = 
	match (branch_structure' b) with
		| `Weight w -> w
		| `Structure m -> total_weight' m
and total_weight' m =
	(branch_weight' (left_branch' m) + branch_weight' (right_branch' m))


let rec is_balanced' m = 
	let iter b = 
		match (branch_structure' b) with
			| `Weight w -> true
			| `Structure c -> is_balanced' c
	in iter (left_branch' m) && iter (right_branch' m) && 
		((branch_length' (left_branch' m)) * branch_weight' (left_branch' m)
		= (branch_length' (right_branch' m)) * branch_weight' (right_branch' m))


(* A.2 *)
type tree = Tree of elem list
and elem = 
	| Num of int
	| Sub of tree
	
let rec square_tree (Tree t) = 
	let rec iter t1 =
		match t1 with
			| [] -> []
			| Num h :: t -> Num(h * h) :: (iter t)
			| Sub h :: t -> Sub(square_tree h) :: (iter t)
	in Tree(iter t)

let rec square_tree' (Tree t) = 
	let rec iter t1 =
		match t1 with
			| Num h -> Num (h * h)
			| Sub s -> Sub (square_tree' s)
	in Tree (List.map iter t)

(* A.3 *)
let tree_map f (Tree t) = 
	let rec iter t1 =
		match t1 with
			| Num h -> Num (f h)
			| Sub s -> Sub (square_tree' s)
	in Tree (List.map iter t)

(* A.4 *)
let rec subsets = function
	| [] -> [[]]
	| h :: t -> let rest = subsets t in rest 
		@ (List.map (fun x -> h :: x) rest)
		
(* This function works because we are finding all of the subsets in the
tail of the list, and then our List.map clause will map each sublist
into a new list that has the head at the front of it. Then, we will also
append this to a list that has all the subsets that do not contain the 
head element of the old list. This combined will give us all possible
subsets of the list, and then it will return the list of lists of subsets
of our original set *)

(* A.5 *)
let rec accumulate op initial sequence =
	match sequence with 
		| [] -> initial
		| h :: t -> op h (accumulate op initial t)
		
let map p sequence = 
	accumulate (fun x r -> (p x) :: r) [] sequence
	
let append seq1 seq2 = 
	accumulate (fun x r -> x :: r) seq2 seq1

let length sequence = 
	accumulate (fun x r -> r + 1) 0 sequence

(* A.6 *)
let rec accumulate_n op init seqs =
	match seqs with
		| [] -> failwith "empty list"
		| [] :: _ -> []
		| h :: t -> accumulate op init (List.map List.hd seqs) ::
			 accumulate_n op init (List.map List.tl seqs)
			
(* A.7 *)
let rec map2 f x y = 
	match (x, y) with
		| ([], []) -> []
		| ([], _) -> failwith "unequal lists"
		| (_, []) -> failwith "unequal lists"
		| (h1 :: t1, h2 :: t2) -> (f h1 h2) :: (map2 f t1 t2)

		
let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map (fun x -> dot_product v x) m

let transpose mat = accumulate_n (fun x y -> x :: y) [] mat 

let matrix_times_matrix m n = 
	let cols = transpose n in 
	map (fun x -> matrix_times_vector cols x) m
	

(* B.1 *)

let rec quicksort lst cmp =
	match lst with
		| [] -> []
		| h :: t -> 
		let first_half = List.filter (fun y -> (cmp y h)) t
		and second_half = List.filter (fun y -> not (cmp y h)) t
		in (quicksort first_half cmp) @ (h :: quicksort second_half cmp)
	
(* B.2 *)
(* Our quicksort algorithm is generative recursion and not structural 
recursion because each time we do our recursion, we generate two new 
halves of the list, and thus we are generating many new lists and
apending them all to finalize the recursion *)

(* B.3 *)

(* B.4 *)
let rec insert_in_order new_result a_list cmp =
	match a_list with
	| [] -> [new_result]
	| h :: t when cmp new_result h -> new_result :: a_list
	| h :: t -> h :: insert_in_order new_result t cmp


let rec insertion_sort a_list cmp =
	match a_list with
	| [] -> []
	| h :: t -> insert_in_order h (insertion_sort t cmp) cmp

(* This is structural recursion *)

(* C.1 *)
type expr = 
	| Int of int
	| Var of string
	| Add of expr * expr
	| Mul of expr * expr
	| Pow of expr * int

let rec simplify1 expr = 
	let rec pow x n = 
		match n with
		| 0 -> 1
		| _ -> x * pow x (n - 1)
	in match expr with
		| Add (Int e1, Int e2) -> Int (e1 + e2)
		| Mul (Int e1, Int e2) -> Int (e1 * e2)
		| Pow (Int e1, e2) -> Int (pow e1 e2)
		| Add (Int 0, e2) -> e2
		| Add (e1, Int 0) -> e1
		| Mul (e1, Int 0) -> Int 0
		| Mul (Int 0, e2) -> Int 0
		| Mul (Int 1, e2) -> e2
		| Mul (e1, Int 1) -> e1
		| Pow (e1, 0) -> Int 1
		| Pow (e1, 1) -> e1
		| Add (e1, e2) -> Add (simplify1 e1, simplify1 e2)
		| Mul (e1, e2) -> Mul (simplify1 e1,  simplify1 e2)
		| Pow (e1, e2) -> Pow (simplify1 e1, e2)
		| Int _ -> expr
		| Var _ -> expr


let rec simplify expr = 
	let e = simplify1 expr in
		if expr = e then expr else simplify e

(* C.2 *)

let rec deriv expr d = 
	match expr with
		| Int _ -> Int 0
		| Var v-> if v = d then Int 1 else Int 0
		| Add (e1, e2) -> Add((deriv e1 d), (deriv e2 d))
		| Mul (e1, e2) -> Add( Mul((deriv e1 d), e2), Mul(e1, (deriv e2 d)))
		| Pow (e1, n) -> Mul(deriv e1 d, (Mul(Int n, Pow(e1, (n-1)))))
	

let derivative expr var = 
	let e = simplify expr in
	let d = deriv e var in
		simplify d
