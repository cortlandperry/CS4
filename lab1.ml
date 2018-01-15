(* A.1 *)
(* - :int = 10 *)

(* A.2 *)
(* - :float = 10. *)

(* A.3 *)
(* - :int = 12 *)

(* A.4 *)
(* Error. This is because we are not using the float-specific '+.' 
operator. *)

(* A.5 *)
(* Error. This is because we are using the float operator when we are
adding ints *)

(* A.6 *)
(* Error. This is because we are adding two different types (float and
int) *)

(* A.7 *)
(* Error. Once again we are adding two different types so we cannot
have the proper operator for both of them *)

(* A.8 *)
(* - :float = 7.2 *)

(* A.9 *)
(* - :int = 5 *)

(* A.10 *)
(* - :int = 7 *)

(* A.11 *)
(* val a: int = 3 *)

(* A.12 *)
(* val b: int = 4 *)

(* A.13 *)
(* - : bool = false *)

(* A.14 *)
(* - : bool = true *)

(* A.15 *)
(* - : bool = false *)
(* This expression is different because it means that the two values 
have the exact same spot in memory, whereas the other one means that
the two are structuarlly equal *)

(* A.16 *)
(* -: (int * int * int) list = [(1, 2, 3)] *)

(* A.17 *)
(* -: (int * int * int) list = [(1, 2, 3)] *)
(* This gives this result because when we have commas between our
values, the ocaml interpreter reads it as if we have a list of 1 tuple
instead of multiple different elements in the list. *)

(* A.18 *)
(* -: int = 4 *)

(* A.19 *)
(* Error. This is because we cannot use the keyword "and" in our if 
statements *)

(* A.20 *)
(* -: int = 6 *)

(* A.21 *)
(* Error. This is because when we run a if statement with no "else"
ocaml automattically gives the else as a unit type and the values that
are returned in the if statement both must be the same and b is not 
unit type thus we get a type error *)

(* A.22 *)
let sum_of_squares_of_two_largest x y z = 
	if x <= y && x <= z then (y * y + z * z)
	else if z <= y && z <= x then (x * x + y * y)
	else (x * x + z * z)

(* A.23 *)
(* This function takes two integers a and b, and if b is positive,
then you add it to a, and if b is negative, then you subtract it from
a *)

(* B.1 *)
(* If the program is using a normal-order interpreter, we will see that
our recursive function p will not run, because the interpreter will
run the first part of test and see x = 0 and then return 0. OTherwise,
if it is using a strict interpreter (applicitive order), we will 
evaluate the recursive function p when we first call test, but it will
not be necesary to return the final result. *)

(* B.2 *)
(* When Alyssya tries to use this function to evaluate square roots,
the machine will first run our new_if() function that was created. That
means it will evaluate the predicate, which will run our function
is_good_enough on our guess and actual x. If it is good enough
(when that returns true), our function will return our guess and that 
will be the square root. If that function returns false, then it will
cause our predicate to be false, and then we will run into the "else"
statement in our new_if. Then we run our function again with a new and 
imporved guess. This will repeat until our "improved guess" satisfies
our conditions in is_good_enough, in which case that will be 
returned. *)

(* B.3 *)
(* I hope this is enough steps lol! *)

(* add_a 2 5
evaluate 2 -> 2
evaluate 5 -> 5
Substitute 2 and 5 for a, b in add_a
	if 2 = 0 
		evaluate 2 -> become 2
		evaluate 0 -> become 0
	evaluate 2 = 0 -> false
	evaluate inc (add_a (dec 2) 5)
	evaluate add_a (dec 2) 5
		evaluate dec 2
			evaluate 2 -> 2
			substitute 2 in for a
			dec 2 = 2 - 1
				evaluate 2 = 2
				evaluate 1 = 1
			dec 2 = 1
		evaluate 5 -> 5
		now substitute 1 and 5 for a and b
		if 1 = 0 
			evaluate 1 ->1
			evaluate 0 -> 0
		1 = 0 -> false
		evaluate inc (add a (dec 1) 5)
			evaluate add a (dec 1) 5
				evaluate dec 1
					evaluate 1 -> 1
					substitute 1 for a
					dec 1 = 1 - 1
					evaluate 1 = 1
					evaluate 1 = 1
				dec 1 = 1- 1 = 0
			evaluate add_a 0 5
			substitute 0 and 5 for a and b in func
			if 0 = 0
				evaluate 0 - > 0
				evaluate 0 -> 0
			0 = 0 returns true
			then 5 -> (add a 0 5) = 5
		evaluate inc (5)
			evaluate 5 to 5
			substitute 5 for a
			inc 5 = 5 + 1
				evaluate 5
				evaluate 1
		inc 5 = 6
	evaluate inc (6)
		evalaute 6 to 6
		substitute 6 for a
		inc 6 = 6 + 1
		evaluate 6 = 6
		evaluate 1 = 1
	inc 6 = 7
Thus, our final answer is 7. 
*)


(*
add_b 2 5
	evaluate 2 and 5 -> 2, 5 
	substitute 2 and 5 in for a and b
	if 2 = 0
		evaluate 2 and 0 -> 2, 0
	if 2 = 0 returns false
	evaluate add_b (dec 2) (inc 5)
		dec 2
			2 evaluates to 2
			substitute 2 for a
			dec 2 = 2 - 1
				evaluate 2 = 2
				evaluate 1 = 1
				evaluate 2 - 1 = 1
			dec 2 = 1
		inc 5 
			5 evaluates to 5
			substitute 5 for a
			inc 5 = 5 + 1
				evalaute 5 = 5
				evaluate 1 = 1
				evalaute 5 + 1 = 6
			then inc 5 = 6
		now we have add_b 1 6
		substitute 1 and 6 in for a and b
			if 1 = 0
				evaluate 1 -> 1
				evaluate 0 -> 0
			1 = 0 returns false
			evaluate add_b (dec 1) (inc 6)
				dec 1
					1 evaluates to 1
					substitute 1 for a
					dec 1 = 1 - 1
						evaluate 1 = 1
						evaluate 1 = 1
						evaluate 1-1 = 0
				dec 1 = 0
				inc 6 
					6 evaluates to 6
					substitute 6 for a
					inc 5 = 5 + 1
						evalaute 6 = 6
						evaluate 1 = 1
						evalaute 6 + 1 = 7
				then inc 6 = 7
			now we have add_b 0 7
				substitute 0 and 7 for a and b
				if 0 = 0 
					evaluate 0, 0 -> both equal 0
				then if returns true
				then return 7 (return b)
			then add_b 0 7 = 7
		then add_b 1 6 = 7
	then add_b 2 5 = 7
*)
(* add_a is a recursive function, add_b is an iterative function *)

(* C.1 *)
let rec factorial n = 
	if n = 0 then 1 else n * factorial (n - 1)

(* C.1.a *)
let e_term x = 
	1.0 /. float_of_int (factorial x)

(* C.1.b *)
let rec e_approximation x = 
	if x = 0 then 1.0 else e_term x +. e_approximation (x - 1)

(* C.1.c *)
(* My function: - : float = 2.71828182845904553 *)
(* exp 1.0: - : float = 2.71828182845904509 *)

(* C.1.d *)
(* When you try to make a better approximation, ocaml will return
inifity as our answer. This happens because ocaml cannot store
values that are as small as 1/100! in our floating point number. *)

(* C.2 *)
let rec is_even x =
	if x = 0 then true else is_odd(x - 1)
and is_odd a =
	if a = 0 then false else is_even(a - 1)

(* C.3 *)
let rec f_rec x =
	if x < 3 then x 
	else f_rec(x - 1) + 2 * f_rec(x - 2) + 3 * f_rec (x - 3)
	
let rec iter_help three_b two_b one_b current max = 
	let cur_val = one_b + 2 * two_b + 3 * three_b in
	if current >= max then cur_val
	else iter_help two_b one_b cur_val (current + 1) max


let f_iter x = 
	if x < 3 then x else iter_help 0 1 2 3 x
	
(* C.4 *)
let rec pascal_coefficient x y = 
	match x, y with
		| i, j when i < 1 || j < 1 -> failwith "invalid arguments"
		| i, j when j > i -> failwith "invalid arguments"
		| 1, i -> 1
		| j, 1 -> 1
		| i, j when i = j -> 1
		| i, j -> pascal_coefficient (x-1) (y-1) + pascal_coefficient (x-1) (y)
