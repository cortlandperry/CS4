(* A.1 *)

(*

FRAME 0 (initial enviornment)
	parent: none
	bindings:
		- : [primitive function -]
		* : [primiitive function *]
		
FUNCTION 0 (fun n -> let rec iter n 1 ...)
	enviornment: FRAME 0
	param: n
	body: let rec iter n 1 ...

FRAME 1 (let factorial n = FUNCTION 0)
	parent: FRAME 0
	bindings:
		factorial : FUNCTION 0
		
FRAME 2 (FUNCTION 0 applied to 3)
	parent: FRAME 0
	bindings:
		n : 3

FUNCTION 1 (fun m r -> if m = 0 ...)
	enviornment: FRAME 3
	param: m r
	body: if m = 0 ...

FRAME 3 (let iter m r = FUNCTION 1)
	parent: FRAME 2
	bindings: 
		iter : FUNCTION 1

FRAME 4 (FUNCTION 1 applied to n 1)
	parent: FRAME 2
	bindings: 
		m : n
		r : 1
		
FRAME 5 (FUNCTION 1 applied to 2 3)
	parent: FRAME 3
	bindings:
		m : 2
		r : 3

FRAME 6 (FUNCTION 1 applied to 1 6)
	parent: FRAME 3
	bindings:
		m : 1
		r : 6

FRAME 7 (FUNCTION 1 applied to 0 6)
	parent: FRAME 3
	bindings: 
		m : 0
		r : 6
		
*)

(* A.2 *) 
let factorial n = 
	let f = ref (fun n -> 0) in
	let a n = 
		if n = 0 then 1
		else n * (!f (n - 1)) in
	f := a;
	!f

(* B.1 *)
exception Stat_error of string

let make_stat_1 arg = 
	let sum = ref (0.) in
	let sumsq = ref (0.) in
	let n = ref (0) in
	object 
		method append x = 
			begin
				sum := !sum +. x;
				sumsq := !sumsq +. (x *. x);
				n := !n + 1;
			end
		method mean = 
			match !n with
			| 0 -> raise (Stat_error "need at least one value for mean")
			| _ -> !sum /. (float_of_int !n)
		method variance = 
			match !n with
			| 0 -> raise (Stat_error "need at least one value for variance")
			| _ -> (!sumsq -. (!sum *. !sum /. (float_of_int !n))) 
				/. (float_of_int !n)		
		method stdev = 
			match !n with
			| 0 -> raise (Stat_error "need at least one value for stdev")
			| _ -> sqrt ((!sumsq -. (!sum *. !sum /. (float_of_int !n))) 
				/. (float_of_int !n))
		method clear = 
			begin
				sum := 0.;
				sumsq := 0.;
				n := 0;
			end		
	end
	

(* B.2 *)
let make_stat_2 arg = 
	let sum = ref (0.) in
	let sumsq = ref (0.) in
	let n = ref (0) in
	object (self)
		method append x = 
			begin
				sum := !sum +. x;
				sumsq := !sumsq +. (x *. x);
				n := !n + 1;
			end
		method mean = 
			match !n with
			| 0 -> raise (Stat_error "need at least one value for mean")
			| _ -> !sum /. (float_of_int !n)
		method private _variance = (!sumsq -. (!sum *. !sum /. (float_of_int !n))) 
				/. (float_of_int !n)
		method variance = 
			match !n with
			| 0 -> raise (Stat_error "need at least one value for variance")
			| _ -> self#_variance		
		method stdev = 
			match !n with
			| 0 -> raise (Stat_error "need at least one value for stdev")
			| _ -> sqrt self#_variance
		method clear = 
			begin
				sum := 0.;
				sumsq := 0.;
				n := 0;
			end		
	end

(* C.1 *)
module type PRIORITY_QUEUE =
    sig
      exception Empty
  
      type elem      (* Abstract type of elements of queue. *)
      type t         (* Abstract type of queue. *)
  
      val empty      : t                (* The empty queue.         *)
      val is_empty   : t -> bool        (* Check if queue is empty. *)
      val insert     : t -> elem -> t   (* Insert item into queue.  *)
      val find_min   : t -> elem        (* Return minimum element.  *)
      val delete_min : t -> t           (* Delete minimum element.  *)
      val from_list  : elem list -> t   (* Convert list to queue.   *)
    end
 
 
module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
	struct
		exception Empty
		type elem = int
		type t = Leaf | Node of int * elem * t * t
		
		let empty = Leaf
		let is_empty h = (h = empty)

		let rec rank t = 
			match t with
				| Leaf -> 0
				| Node (r, e, t1, t2) -> r
		let rec merge h1 h2 = 
			let new_heap h1 h2 min =
				if rank h1 < rank h2
				then Node((rank h1) + 1, min, h2, h1)
				else Node((rank h2) + 1, min, h1, h2)
			in
			match (h1, h2) with
			| (_, Leaf)-> h1
			| (Leaf, _) -> h2
			| (Node(rank1, e1, l1, r1), Node(rank2, e2, l2, r2)) ->
				if e1 < e2 then new_heap l1 (merge r1 h2) e1
				else new_heap l2 (merge h1 r2) e2
		let insert h e = 
			merge h (Node (1, e, Leaf, Leaf))
		let find_min h = 
			match h with
			| Leaf -> raise Empty
			| Node (r, e, t1, t2) -> e
		let delete_min h = 
			match h with
			| Leaf -> raise Empty
			| Node (r, e, t1, t2) -> merge t1 t2
		let rec from_list l = 
			match l with
			| [] -> Leaf
			| h :: t -> insert (from_list t) h
	
	end
	
let heap_sort l = 
	let rec iter heap lst = 
		if PriorityQueue.is_empty heap then List.rev lst
		else iter (PriorityQueue.delete_min heap) ((PriorityQueue.find_min heap) :: lst)
	in iter (PriorityQueue.from_list l) []

(* C.2 *)
(* Type for ordered comparisons. *)
type comparison = LT | EQ | GT

(* Signature for ordered objects. *)
module type ORDERED =
	sig
	  type t
	  val cmp: t -> t -> comparison
	end

module OrderedString =
    struct
      type t = string
      let cmp x y = 
        if x = y then EQ else if x < y then LT else GT
    end


module MakePriorityQueue (Elt : ORDERED) 
	: (PRIORITY_QUEUE with type elem = Elt.t) =
	struct
		exception Empty
		type elem = Elt.t
		type t = Leaf | Node of int * elem * t * t
		
		let empty = Leaf
		let is_empty h = (h = empty)

		let rec rank t = 
			match t with
				| Leaf -> 0
				| Node (r, e, t1, t2) -> r
		let rec merge h1 h2 = 
			let new_heap h1 h2 min =
				if rank h1 < rank h2
				then Node((rank h1) + 1, min, h2, h1)
				else Node((rank h2) + 1, min, h1, h2)
			in
			match (h1, h2) with
			| (_, Leaf)-> h1
			| (Leaf, _) -> h2
			| (Node(rank1, e1, l1, r1), Node(rank2, e2, l2, r2)) ->
				if (Elt.cmp e1 e2) = LT
				then new_heap l1 (merge r1 h2) e1
				else new_heap l2 (merge h1 r2) e2
		let insert h e = 
			merge h (Node (1, e, Leaf, Leaf))
		let find_min h = 
			match h with
			| Leaf -> raise Empty
			| Node (r, e, t1, t2) -> e
		let delete_min h = 
			match h with
			| Leaf -> raise Empty
			| Node (r, e, t1, t2) -> merge t1 t2
		let rec from_list l = 
			match l with
			| [] -> Leaf
			| h :: t -> insert (from_list t) h
	end

 
module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 s = 
	let rec iter heap str = 
		if StringPQ.is_empty heap then List.rev str
		else iter (StringPQ.delete_min heap) ((StringPQ.find_min heap) :: str)
	in iter (StringPQ.from_list s) []
	
(* D.1 *)
type 'a contents = Result of 'a | Expr of (unit -> 'a)
type 'a lazy_t = 'a contents ref

let make_lazy e = ref (Expr e) 

let force lz = 
	match !lz with
	| Result r -> r
	| Expr e -> e ()
 
(* D.2.1 *)
let y = 
    fun f -> 
      (fun z -> z (`Roll z)) 
      (fun (`Roll w) -> f (fun x -> w (`Roll w) x))
     
let almost_sum = 
	fun f ->
		function
		| [] -> 0
		| h :: t -> h + f t

let sum = y almost_sum

(* D.2.2 *)

let factorial2 n = 
	let iter = 
		fun f ->
			fun n->
				fun r ->
				if n = 0 then r else f (n - 1) (r * n)	
    in 
    let fac = y iter
    in fac n 1


