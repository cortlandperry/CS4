(* A.1 *)
type point = {x: float; y: float}

type segment = {startp: point; endp: point}

let midpoint_segment {startp = p1; endp = p2} = 
	{x = ((p1.x +. p2.x) /. 2.0) ; y = ((p1.y +. p2.y) /. 2.0)}

let segment_length {startp = p1; endp = p2} = 
	sqrt ((p1.x -. p2.x)**2.0 +. (p1.y -. p2.y)**2.0)
	
let print_point {x = x; y = y} =
	Printf.printf "(%g, %g)\n" x y

let make_point x1 y1 = 
	{x = x1 ; y = y1}

let make_segment p1 p2 =
	{startp = p1 ; endp = p2}

let get_coords point =
	(point.x, point.y)

let get_points segment = 
	(segment.startp, segment.endp )

(* A.2 *)
type rectangle = {bl: point; tr: point}

let rectangle_lower_segment {bl = p1; tr = p2}=
	let created_point = make_point p2.x p1.y in
	make_segment p1 created_point

let rectangle_upper_segment {bl = p1; tr = p2} = 
	let created_point = make_point p1.x p2.y in
	make_segment created_point p2

let rectangle_left_segment {bl = p1; tr = p2} = 
	let created_point = make_point p1.x p2.y in
	make_segment p1 created_point

let rectangle_right_segment {bl = p1; tr = p2} =
	let created_point = make_point p2.x p1.y in
	make_segment created_point p2
	
let rectangle_perimeter rect =
	let right = segment_length (rectangle_right_segment rect) in
	let lower = segment_length (rectangle_lower_segment rect) in
	(2.0 *. right) +. (2.0 *.lower)

let rectangle_area rect = 
	let right = segment_length (rectangle_right_segment rect) in
	let lower = segment_length (rectangle_lower_segment rect) in
	lower *. right

let make_rectangle p1 p2 =
	{bl = p1; tr = p2}

type rectangle2 = {x1: float; x2: float; y1: float; y2: float}

let rectangle_lower_segment2 {x1; x2; y1; y2} = 
	let p1 = make_point x1 y1 in
	let p2 = make_point x2 y1 in
	make_segment p1 p2
	
let rectangle_upper_segment2 {x1; x2; y1; y2} = 
	let p1 = make_point x1 y2 in
	let p2 = make_point x2 y2 in
	make_segment p1 p2

let rectangle_left_segment2 {x1; x2; y1; y2} = 
	let p1 = make_point x1 y1 in
	let p2 = make_point x1 y2 in
	make_segment p1 p2

let rectangle_right_segment2 {x1; x2; y1; y2} = 
	let p1 = make_point x2 y1 in
	let p2 = make_point x2 y2 in
	make_segment p1 p2

let rectangle_perimeter2 rect =
	let right = segment_length (rectangle_right_segment2 rect) in
	let lower = segment_length (rectangle_lower_segment2 rect) in
	(2.0 *. right) +. (2.0 *.lower)

let rectangle_area2 rect = 
	let right = segment_length (rectangle_right_segment2 rect) in
	let lower = segment_length (rectangle_lower_segment2 rect) in
	lower *. right

let make_rectangle2 x1 x2 y1 y2 =
	{x1 = x1; x2 = x2; y1 = y1; y2 = y2}
	
(* A.3 *)
let make_pair x y = fun m -> m x y

let first z = z (fun x y -> x)

let second z = z (fun x y -> y)

(* Evaluate first (make_pair x y)
first we evaluate make_pair (x y)
this creates a pair with the values x and y
evaluate first (pair)
first(pair) = pair (fun x y -> x)
this will return the first element of the pair of numbers given
thus, we will return x, because our pair is x, y
*)

(* Evaluate second (make_pair 1 2)
evaluate make_pair 1 2
evaluate 1 -> 1 2 -> 2
substitute 1 and 2 in make_pair:
	fun m -> m 1 2
	now apply fun m to our pair
	we return pair 1 2 
now evalaute second (pair 1 2)
evaluate pair 1 2 -> pair 1 2
substitute our values for z into second
	we get second (pair 1 2) = (pair 1 2) (fun x y -> y)
	apply fun x y to 1 2
	substitute 1 2 into our function
	1 2 -> 2
	return 2
	thus, our result is 2
*)

(* A.4 *)

let pow a b = 
	int_of_float ((float_of_int a) ** (float_of_int b)) 
	
let int_log a b =
	let rec iter a1 b1 count = 
		if b1 mod a1 = 0 then iter a1 (b1 / a1) (count + 1)
		else count
	in iter a b 0

let make_pairi x y = (pow 2 x) * (pow 3 y)

let firsti z = int_log 2 z

let secondi z = int_log 3 z

(* A.5 *)
let zero = []

let is_zero = function
	| [] -> true
	| () :: _ -> false

let succ u = () :: u

let prev u = 
	match u with
	| [] -> invalid_arg "This Value is 0"
	| h::t -> t

let integer_to_unary x =
	let rec iter u_rep x = 
		if x = 0 then u_rep else iter (succ u_rep) (x - 1)
	in iter zero x

let unary_to_integer u_rep =
	let rec iter u_rep x = 
		if is_zero u_rep then x else iter (prev u_rep) (x + 1)
	in iter u_rep 0
	
let unary_add a b = 
	integer_to_unary(unary_to_integer a + unary_to_integer b)

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
	|Zero -> true
	|Succ _ -> false

let succ' u = Succ u

let prev' u =
	match u with
		|Zero -> invalid_arg "The value in Prev is 0"
		|Succ n -> n

(* We do not need to change how we wrote our integer to unary and unary
to integer functions, other than changing to prime versions *)
(*however, to test i still wrote them in here *)
let integer_to_unary' x =
	let rec iter u_rep x = 
		if x = 0 then u_rep else iter (succ' u_rep) (x - 1)
	in iter zero' x

let unary_to_integer' u_rep =
	let rec iter u_rep x = 
		if is_zero' u_rep then x else iter (prev' u_rep) (x + 1)
	in iter u_rep 0
	
let unary_add' a b = 
	integer_to_unary'(unary_to_integer' a + unary_to_integer' b)
	
(* A.6 *)
let zero = fun s -> fun z -> z

let add1 n = fun s -> fun z -> s (n s z)

let one s z = s z
let two s z = s (s z)
let three s z = s (s (s z))
let four s z = s (s (s (s z)))
let five s z = s (s (s (s (s z))))
let six s z = s (s (s (s (s (s z)))))
let seven s z = s (s (s (s (s (s (s z))))))
let eight s z = s (s (s (s (s (s (s (s z)))))))
let nine s z = s (s (s (s (s (s (s (s (s z))))))))
let ten s z = s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = 
	m s (n s z)

let church_to_integer n = 
	n (fun x -> x + 1) 0
	
(* A.7 *)
(* looking at the type signatures for church_to_integer, zero and one. 
First see that the type sig of zero is val zero : 'a -> 'b -> 'b
if we look at zero inputted into church_to_integer, we get ((int ->
int) -> int -> a') -> a'. So when we compare the type signatures, 
we can see that the a' in zero is replaced by (int -> int), which is 
what we know s is. Similarly, we can place the second argument into
the type signature of church_to_integer, and we see the second argument
of zero is type int. This makes the result an int because the last 
argument is the same as the second argument. (b', and b').

Now, if we look at one, we can use a similar argument to zero.
The type signature of one is val one : ('a -> 'b) -> 'a -> 'b
we know then (a' -> b') is (int -> int). so that means both a and b
are int, and thus our return statement for one is also int.

*)

(* B.1 *)
let rec last_sublist = function
	| [] -> invalid_arg "last_sublist: empty list"
	| [i] -> [i]
	| h :: t -> last_sublist t
	
(* B.2 *)
let reverse lst = 
	let rec iter newlst oldlst =
		match oldlst with
		|[] -> newlst
		|h::t -> iter (h::newlst) t
	in iter [] lst


(* B.3 *)
let rec square_list = function
	|[] -> []
	| h::t -> h*h :: square_list t

let square_list2 items = List.map (fun x -> x * x) items

(* B.4 *)
(* The first implementation doesnt work because it takes the element
from the front of the list and then puts it at the front of the list
that we will end up returning, thus the last element of the original
list ends up being the first element of the results list *)

(* This new one is adding lists to the results list instead of adding 
elements, so our resulting list will be a list of lists, instead of 
a list of elements. Also, this code will also return in a reversed order
for the same reason as the previous example. If we want to fix this,
we can change the :: to the @ operator and have it be [h * h] instead of
( h * h). This new method will not be efficient, because it uses the @
operator, and if we want to make it efficient we use the rev_append 
function instead *)

let square_list3 items = 
	let rec iter things answer = 
		match things with
		| [] -> answer
		| h :: t -> iter t (answer @ [h * h])
	in iter items []

(* B.5.1 *)
let count_negative_numbers lst =
	let rec iter lst count =
		match lst with
			| [] -> count
			| h::t when h < 0 -> iter t (count + 1)
			| h::t -> iter t count
	in iter lst 0

(* B.5.2 *)
let power_of_two_list n = 
	let rec iter lst counter =
		match counter with
			| _ when counter < 0 -> []
			| 0 -> 1 :: lst
			| _ -> (iter ([pow 2 counter] @ lst) (counter - 1)) 
	in iter [] (n - 1)
	
(* B.5.3 *)
let prefix_sum lst = 
	let rec iter sum lst results =
		match lst with
			| [] -> results
			| h::t -> iter (sum + h) t (results@[sum + h])
	in iter 0 lst []

(* B.6 *)
let deep_reverse lst =
	let rec iter newlst oldlst =
		match oldlst with
			| [] -> reverse newlst
			| h::t -> (iter newlst t)@[(reverse h)]
	in iter [] lst

(* B.7 *)
type 'a nested_list = 
	|Value of 'a 
	|List of 'a nested_list list

let rec deep_reverse_nested lst = 
	let reversal l = 
		let rec iter lst1 result = 
			match lst1 with
			| [] -> result
			| h::t -> iter t ((deep_reverse_nested h) :: result)
		in iter l []
	in
	match lst with
	| Value a -> Value a
	| List lst1 -> List (reversal lst1)
	

