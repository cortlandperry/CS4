(* A.1 *)
let fibonacci n = 
	let n1 = ref 0 in
	let n2 = ref 1 in
	let result = ref (0) in
	let count = ref 0 in
	while !count < n do
		n1 := !n2;
		n2 := !result;
		result := (!n1 + !n2);
		count := !count + 1
	done;
	!result

let fibonacci2 n = 
	let n1 = ref 0 in
	let n2 = ref 1 in
	let result = ref (0) in
	for i = 0 to (n - 1) do 
		n1 := !n2;
		n2 := !result;
		result := (!n1 + !n2);
	done;
	!result
	
(* A.2 *)
let bubble_sort array = 
	let len = ref (Array.length array) in
	for i = 0 to (!len - 1) do
		for j = 0 to (!len - 2) do
			if (array.(j) > array.(j + 1)) then
				let temp = ref array.(j) in
				array.(j) <- array.(j + 1);
				array.(j + 1) <- !temp
		done
	done
	
(* B.a *)
let meters_per_foot = 0.3048

let get_meters len = 
	match len with
	| `Meter m -> m
	| `Foot f -> f *. meters_per_foot
	| `Inch i -> i *. meters_per_foot /. 12.

let length_add a b = `Meter (get_meters a +. get_meters b) 
		
(* B.b *)
let grams_per_slug = 14593.903203

let get_grams mass = 
	match mass with
	| `Gram g -> g
	| `Kilo k -> k *. 1000.
	| `Slug s -> s *. grams_per_slug

let mass_add a b = `Gram (get_grams a +. get_grams b)

let get_seconds time =
	match time with 
	| `Second s -> s
	| `Minute m -> m *. 60.
	| `Hour h -> h *. 60. *. 60.
	| `Day d -> d *. 24. *. 60. *. 60.
	
let time_add a b = `Second (get_seconds a +. get_seconds b)

(* B.c *)
let unit_add a b = 
	match (a, b) with
	| (`Length l1, `Length l2) -> `Length (length_add l1 l2)
	| (`Mass m1, `Mass m2) -> `Mass (mass_add m1 m2)
	| (`Time t1, `Time t2) -> `Time (time_add t1 t2)
	| _ -> failwith "values not compatible"

(* No, we do not get an explosion because our function just checks
if it is one of the 3 cases that are valid, and if it is not one
of those 3 cases then it fails and stops, because it doesn't 
try to add unaddable things *)

(* C.1 *)
let rec make_gram g = 
	let helper a = a#unit_type = `Gram || a#unit_type = `Slug in
	object
		method get_grams = g
		method unit_type = `Gram
		method get_slugs = g /. grams_per_slug
		method compatible other = helper other
		method add other = if helper other
			then make_gram (g +. other#get_grams)
			else failwith "values not compatible"
	end

(* C.2 *)

(* Starting Code Base *)

(* Define a number as a message-passing object. *)
(* "i" is an int. *)
let rec make_number i =
object
  method value = i
  method show = string_of_int i
  method is_zero = i = 0
  method is_number = true
  method evaluate _ _ = make_number i  (* must evaluate to an object *)
  method derive _ = make_number 0  (* derivative of a number is 0 *)
end

(* Define a variable as a message-passing object. *)
(* "v" is a string. *)
let rec make_variable v =
object
  method value = failwith "variable has no numerical value"
  method show  = v
  method is_zero = false
  method is_number = false
  method evaluate v' n =
	if v = v'
	  then make_number n
	  else make_variable v
  method derive v' =
	if v = v'
	  then make_number 1  (* d/dx(x) = 1 *)
	  else make_number 0  (* d/dx(y) = 0 *)
end

(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
match () with
  | _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
  | _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
  | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
		make_number (expr1#value + expr2#value)
  | _ ->  (* create a new object representing the sum *)
		object
		  method value = failwith "sum expression has no numerical value"
		  method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
		  method is_zero = false
		  method is_number = false
		  method evaluate v n = 
			make_sum (expr1#evaluate v n) (expr2#evaluate v n)
		  method derive v = 
			make_sum (expr1#derive v) (expr2#derive v)
		end

(* Evaluate a message-passing expression with a number 
 substituted for a variable. *)
let evaluate expr v n = expr#evaluate v n

(* Return the string representation of an expression. *)
let show expr = expr#show

(* Return the derivative of an expression. *)
let differentiate expr v = expr#derive v


(* C.2.a *)
let rec make_product expr1 expr2 = 
	match () with
	| _ when expr1#is_number && expr1#value = 1 -> expr2
	| _ when expr2#is_number && expr2#value = 1 -> expr1
	| _ when expr1#is_zero -> make_number 0
	| _ when expr2#is_zero -> make_number 0
	| _ when expr1#is_number && expr2#is_number -> 
		make_number (expr1#value * expr2#value)
	| _ -> (* create new object representing the product *)
		object
			method value = failwith "product has no numerical value"
			method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
			method is_zero = false
			method is_number = false
			method evaluate v n = 
				make_product (expr1#evaluate v n) (expr2#evaluate v n)
			method derive v = 
				make_sum 
				(make_product (expr1#derive v) expr2)
				(make_product expr1 (expr2#derive v))
		end

(* C.2.b.1 *)

(* This was returned by the interpreted:
val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)

(* C.2.b.2 *)
(* This was returned by the interpreter:
val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)

(* C.2.b.3 *)
(* This was returned by the interpreter: 
"(((x * (x * y)) + (x * ((x * y) + (x * y)))) + (3 * ((x * (y * y)) + (x * (y * y)))))"
*)

(* C.2.b.4 *)
(* This was returned by the interpreter: 
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))"
*)

(* C.2.b.5 *)
(* This was returned by the interpreter: 
- : string = "558"
*)

(* C.2.b.6 *)
(* This was returned by the interpreter: 
- : string = "396"
*)


