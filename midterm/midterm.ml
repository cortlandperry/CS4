(* 1.a *)
(* This function has time complexity O(n + 1) *)
(* This is because we have an iterative function inside that starts at
n 0, and incrememnts downward one each time it is called. Thus, we will
call the function aux n + 1 times and then run one computation per
iteration, thus we get a time complexity of O(n) *)

(* 1.b *)
(* This function has time complexity O(|m - n|) *)

(* This is because our function will run until m is equal to n. If m is 
less than n, we increment it by one each time we call
our function until we reach n, which is a total of (m-n) calls to the
function, each one  and if it is great than we decrease it we get 
(n - m) total calls to the function. Thus, the total times this runs is 
the absolute value of m -n which is then our complexity *)

(* 1.c *)

(* This function has time complexity O(3^(n+1) / 2) *)

(* We can determine this by using a similar teqniuqe to that used in 
lecture for the fibonacci sequence. We know each time the function runs,
it is a build in function and it takes O(1) to call one instance of the 
function, so T = 1. We then see that 
T (trip n) = T + T(trip n-1) + T(trip n-2) + T(trip n-3)
because it calls itself 3 times each time it runs
We know that the time complexity of T(trip n-1) is greater than
T(trip n-2) and n-3, so, we can say:
T (trip n) <= T + 3T(trip n-1)
Then we will continue simplifying this out each time we call our func,
so we get:
T (trip n) <= T + 3(T + 3T(trip n-2)) <= T + 3T + 9T + ...
Which we can simiplfy to T * 3^(n+1) /2.
Since we know T is one, then an upper bound for our time complexity is
then O(3^(n+1) / 2) *)

(* 1.d *)
(* This function has time complexity O(log(n^2)) *)

(* The worst-case time scenario for this function would be when we have
to only divide our function by two, and every time we divide by two we
result in an odd number that is not divisible by three so then we 
subtract 1. In this case, we will have essentially two steps taken every
 time we "divide" by two. Since if we take n and divide it by two each 
time, but each time it take 2 steps, our total time complexity would 
then be 2 * log n = O(log n^2). *)

(* 1.e *)
(* This function is impossible to anyalze for us because of the final
statement in our pattern matching. This is because we call our function
and increment n by to 3n + 1. This increases our function more than we
decrease by (by dividing by 2), and thus makes us unable to determine
when and if the function terminates. Only very unique values for n can
be solved by this function, because it is possible that 3n + 1 always
makes our input grow icompared to dividing by 2. Thus, we cannot
determine this functions run time *)


(* 2.1 *)
let rec group3 lst = 
	match lst with
	| h1::h2::h3::t -> [(h1, h2, h3)] @ (group3 (h2::h3::t))
	| _ -> []
	
let avg3 lst =
	let grouped = group3 lst 
	in List.map (fun (x, y, z) -> (x + y + z) / 3) grouped
	
(* 2.2 *)
(* 2.2.1 *)
let take n lst = 
	let rec iter n lst head =
		match lst with
		| _ when n = 0 -> (head, lst)
		| [] -> (head, [])
		| h::t -> iter (n - 1) t (head @ [h])
	in  
	match n with
		| _ when n < 0 -> invalid_arg "take"
		| _ -> iter n lst []
	
(* 2.2.2 *)
let take2 n lst = 
	let get_first (x, y) = x
	and get_second (x, y) = y
	and firstlst = (take n lst)
	in
	match firstlst with
	| (a, []) -> (a, [], [])
	| (a, b) -> (a, get_first (take n b), get_second (take n b))

(* 2.2.3 *)
let rec merge lst1 lst2 = 
	match (lst1, lst2) with 
	| ([], l)
	| (l, []) -> l
	| (h1 :: t1, h2 :: t2) -> 
		if h1 < h2 
			then h1 :: merge t1 lst2
			else h2 :: merge lst1 t2
	
let rec mergeN n lst =
	match n with
	| _  when n <= 0 -> invalid_arg "mergeN"
	| _ ->
		match (take2 n lst) with
		| ([], [], []) -> []
		| (a, b, c) -> (merge a b) @ (mergeN n c)

(* 2.2.4 *)
let merge_sort lst = 
	let length = List.length lst
	in let rec iter block lst =
		match block with
		| _ when block > length -> lst
		| _ -> iter (block * 2) (mergeN block lst)
	in iter 1 lst
	
(* 3.1.1 *)
let filteri f lst = 
	let rec iter i lst =
		match lst with
		| [] -> []
		| h :: t -> 
			if f i then h :: (iter (i + 1) t) else (iter (i + 1) t)
	in iter 0 lst

(* 3.1.2 *)
let filteriv f lst = 
	let rec iter i lst = 
		match lst with
		| [] -> []
		| h :: t -> 
			if f i h then h :: (iter (i + 1) t) else (iter (i + 1) t)
	in iter 0 lst

(* 3.1.3 *)
let filteri' f lst =
	filteriv (fun x h -> f x) lst
	
(* 3.2 *)
let bests_so_far f lst =
	let rec iter l best = 
		match l with
		| [] -> []
		| h :: t -> 
			if f h best then h :: (iter t h) else (iter t best)
	in match lst with
		| [] -> failwith "no best element"
		| h :: t -> h :: (iter lst h)

(* 3.3.1 *)
let contfrac a b n = 
	let rec helper x = 
		match x with 
		| _ when n = 0 -> (b 0)
		| 0 -> (b x) +. (helper (x + 1))
		| _ when x = n -> ((a x) /. (b x))
		| _ -> ((a x) /. ((b x) +. (helper (x + 1))))
	in helper 0
		
		

(* 3.3.2 *)
let golden_ratio_terms n = 
	let a x = 1. and b x = 1.
	in contfrac a b n

let pi_terms n = 
	let a x = if x = 1 then 4. else ((float_of_int n) -. 1.) *. ((float_of_int n) -. 1.)
	and b x = if x = 0 then 0. else (2. *. (float_of_int n)) -. 1.
	in contfrac a b n

let e_terms n = 
	let a x = if x = 1 then 1. else ((float_of_int n) -. 1.)
	and b x = if x = 0 then 2. else (float_of_int n)
	in contfrac a b n

(* 3.3.3 *)
let converge f tolerance =
	let rec iter terms prev tolerance  = 
		if (abs_float (prev -. (f terms))) < tolerance 
		then (f terms) else iter (terms * 10) (f terms) tolerance
	in iter 10 (f 1) tolerance
	
(* 4.1 *)
type tree = 
	| Leaf 
	| Node of int * int * tree * tree
	
let depth = function
	| Leaf -> 0
	| Node (d, _, _, _) -> d
	
let data = function
	| Leaf -> failwith "no data"
	| Node (_, v, _, _) -> v

let make_node v l r = 
	let d = 1 + max (depth l) (depth r) in
		Node (d, v, l, r)
		
let rec search v tree =
	match tree with
		| Leaf -> false
		| Node (_, v1, l, r) when v1 = v -> true
		| Node(_, v1, l, r) when v1 < v  -> (search v r)
		| Node(_, v1, l, r) -> (search v l)
	
		
(* 4.2 *)
let left_rotate tree = 
	match tree with
	| Node(_, v, l, Node(_, v3, rl, rr)) -> make_node v3 (make_node v l rl) rr
	| _ -> failwith "can't left rotate"
	
let right_rotate tree = 
	match tree with
	| Node( _ , v, Node(_, v2, ll, lr), r) -> make_node v2 ll (make_node v lr r)
	| _ -> failwith "can't right rotate"

(* 4.3 *)
 let rec insert v t =
    match t with
      | Leaf -> Node (1, v, Leaf, Leaf)  (* base case *)
      | Node (_, v', l, r) ->
        begin
          match () with
            | _ when v < v' ->   (* insert into left subtree *)
              let l' = insert v l in  (* new left subtree *)
                if depth l' - depth r = 2  (* tree is now unbalanced *)
                  then
                    if v < data l'
                      then  (* left-left case *)					
                        (* new value v is in the left subtree of l';
                           need to do a right rotation of the new tree *)
                        
						right_rotate (make_node v' l' r)
                      else  (* left-right case *)
                        (* new value v is in the right subtree of l';
                           need to do a left rotation on l'
                           and a right rotation on the resulting tree. *)
                        right_rotate (make_node v' (left_rotate l') r)
                  else 
                    make_node v' l' r  (* already balanced *)
            | _ when v > v' ->   (* insert into right subtree *)
                let r' = insert v r in
					if depth r' - depth l = 2
						then
							if v < data r' 
								then (* right - left case *)
									left_rotate (make_node v' l (right_rotate r'))
								else (* right-right case *)
									left_rotate (make_node v' l r')
						else
							make_node v' l r' (* already balanced *)
            | _ -> t  (* already in tree *)
        end
        
(* Find the minimum value in an AVL tree. *)
let rec min_avl_tree = function
| Leaf -> None
| Node (_, v, l, _) -> 
  begin
	match min_avl_tree l with
	  | None -> Some v
	  | Some l' -> Some l'
  end
  
  (* Find the maximum value in an AVL tree. *)
let rec max_avl_tree = function
| Leaf -> None
| Node (_, v, _, r) -> 
  begin
	match max_avl_tree r with
	  | None -> Some v
	  | Some r' -> Some r'
  end
  
(* Return true if a tree is a valid AVL tree. *)
let rec is_avl_tree = function
| Leaf -> true
| Node (d, v, l, r) ->
  let dl = depth l in
  let dr = depth r in
  if is_avl_tree l 
	&& is_avl_tree r 
	&& d = 1 + max dl dr
	&& abs (dl - dr) < 2
	then  (* check order invariant *)
	  match (max_avl_tree l, min_avl_tree r) with
		| (None, None) -> true
		| (None, Some rmin) -> v <= rmin
		| (Some lmax, None) -> v >= lmax
		| (Some lmax, Some rmin) -> v >= lmax && v <= rmin
	else false	

