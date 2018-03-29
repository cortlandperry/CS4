open Num

(* A.1 *)

(* It's space complexity is O(n). This is because in order to compute
f(n), we just need to store the result of f(n), f(n-1), .... once each
time to calculate it. Thus, we only need to store the values of n
computations, and thus it only takes o(n) space to run recursive
fibonnacci. *)

(* A.2.a *)

(* the function p will be applied 5 times when sine 12.15 is evaluated*)

(* A.2.b *)

(* Both the time and space complexity of this function 
is O(log base 3 a/10), and thus O(log base 3 a) or O(log a). This is because each 
call to sine calls sine again one time if angle > .1, and each function 
will be different in the chain, so we must store the value of that result 
for each new call to sine. The function will be called this many times 
because we know that we will be dividing a by three n times until it is
less than .1, and to find the number of times we can divde by 3 we take 
log base 3 of a *)

(* A.3.1 *)
let rec fast_expt b n = 
	let is_even m = m mod 2 = 0 in
	let square m = m * m in
		match n with
			| 0 -> 1
			| j when is_even j -> square (fast_expt b (n / 2))
			| _ -> b * fast_expt b (n - 1)


(* A.3.2 *)

let ifast_expt b n = 
	let square m = m * m in 
	let is_even m = m mod 2 = 0 in
	let rec iter a b n = 
		match n with
			| 0 -> a
			| j when is_even j ->  iter a (square b) (n / 2)
			| _ -> iter (a * b) b (n - 1)
	in iter 1 b n

(* A.4 *)
let rec fast_mult a b =
	let double m = m + m in
	let halve m = m / 2 in
	let is_even m = m mod 2 = 0 in
		match a with
			| 0 -> 0
			| 1 -> b
			| j when is_even j -> fast_mult (halve a) (double b)
			| _ -> b + fast_mult (a - 1) b

(* A.5 *)
let ifast_mult b n = 
	let halve m = m / 2 in
	let is_even m = m mod 2 = 0 in
	let rec iter a b n = 
		match n with 
			| 0 -> a
			| j when is_even j -> iter (a + (halve (b * n))) b (halve n)
			| _ -> iter (a + b) b (n - 1)
	in iter 0 b n 
	
(* A.6 *)

(* The worst case space complexity of foo is O(log(n)). This is
because we know f can be computed in constant time and space, 
so every time it is called for large n it does not affect our space.
Also, we need to call foo logn times, because each time we divide our
argument by 2, and we call only foo after we divide our value by two
we will get to less than 1 in logn steps then. Our time complexity 
will be O(n), because each time we may divide our values by two,
but we have to call our function n times, because we divide our
value by two but multiply the amount of times we call it by two 
because we have to calculate it twice per recursion. *)

(* A.7.a *)
(* This is a linear recursive process, because whenever we call
fib n , the function calls the recursive function last_two, which just
calls itself once if the base case is not met. It is not iterative
because each call to the recursive function creates deffered
operations. *)

(* A.7.b *)
(* both the space and time complexity of this function is O(n) *)

(* B.1.a *)
(* (fun x y -> x * (2 + y)) 20 (2 * 4) *)

(* B.1.b *)
(* (fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0 *)

(* B.1.c *)
(* (fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1 *)

(* B.1.d *)
(* (fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1 *)
		
(* B.2 *)
(* First desugar the values: *)
(* (fun x y -> let y = 14 in let z = 22 in x * y * z) (2 * 10) (3 + 4)
  -> (fun x y -> (fun y -> let z = 22 in x * y * z) 14) (2 * 10) (3 + 4)
  -> (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)
 *)
(* Now, we will do the substitution model *)
(* 
evaluate 2 * 10 -> 20
evaluate 3 + 4 -> 7
evaluate fun 20 7 
subtitute 20 7 in for x and y
	(fun y -> (fun z -> 20 * y * z) 22) 14)
	evaluate fun 14 
	substitute 14 in for y 
		(fun z -> 20 * 14 * z) 22
		evalute fun 22
		substitute 22 for z
		fun 22 -> 20 * 14 * 22
		evaluate 20 * 14 * 22
	RESULT: 6160
	
Note: when we run this code, we will not use (3 + 4) for y, because
it is shielded and we do not use that value in our code.

*)

(* B.3 *)
(* (fun x y z -> x + y + z) 10 (x * 2) (y + 3)
Expressions in ocaml are evaluated by first evaluating the arguments to
the function. Thus, when we evaluate each individual argument, there is
no value for x when we try to evaluate (x * 2) argument, thus it 
becomes an unbound value. This can be easily fixed by using let/in
statements instead of let/and statements such as:
let x = 10 
in y = x * 2 
in z = y + 3 
in x + y + z*)

(* C.1 *)
let ni = num_of_int

let isum term a next b = 
	let rec iter a result =
		if a > b then result
		else iter (next a) (result +/ term a)
	in
	iter a (ni 0)
	
(* C.2.a *)
let rec product_rec term a next b = 
	if a > b then (ni 1)
	else term a */ (product_rec term (next a) next b)
		
let rec factorial_rec a =
	let term x = x in
	let next x = x +/ (ni 1) in
	product_rec term (ni 1) next a
	
let pi_product n = 
	let is_even n = mod_num n (ni 2) = (ni 0) in
	let numer n = if is_even n then n +/ (ni 2) else n +/ (ni 1) in
	let denom n = if is_even n then n +/ (ni 1) else n +/ (ni 2) in
	let next n = n +/ (ni 1) in
		(ni 4) */ (product_rec numer (ni 1) next n) // 
		(product_rec denom (ni 1) next n)
		

let pi_approx = float_of_num (pi_product (ni 1000))
	
(* C.2.b *)
let product_iter term a next b =
	let rec iter a result =
		if a > b then result
		else iter (next a) (result */ term a)
	in iter a (ni 1)
	
let factorial_iter a =
	let term x = x in
	let next x = x +/ (ni 1) in
	product_iter term (ni 1) next a

(* C.3.a *)
let rec accumulate_rec combiner null_value term a next b =
	if (a > b) then null_value
	else combiner 
	(term a) (accumulate_rec combiner null_value term (next a) next b)

let sum term a next b = 
	accumulate_rec ( +/ ) (ni 0) term a next b
	
let product term a next b = 
	accumulate_rec ( */ ) (ni 1) term a next b

(* C.3.b *)
let accumulate_iter combiner null_value term a next b =
	let rec iter a result =
		if a > b then result
		else iter (next a) (combiner result (term a))
	in iter a null_value

(* C.4 *)
let compose fun1 fun2 = 
	fun a -> fun1 (fun2 a)
	
(* C.5 *)
let repeated func times =
	let term x = x in
	let rec iter cur term times =
		if cur > times then term
		else iter (cur + 1) (compose term func) times
	in iter 1 term times
	
	
(* C.6 *)
let smooth func dx = 
	fun x -> (func (x +. dx) +. func x +. func (x -. dx)) /. 3.0
	
let nsmoothed func n dx = 
	let smoothed f = 
		fun x -> (f (x +. dx) +. f x +. f (x -. dx)) /. 3.0 
	in (repeated smoothed n) func
	
(* D.1 *)
let is_prime n = 
	if n <= 1 then false
	else 
		let rec iter x =
			match x with
			| 1 -> true
			| x' when n mod x' = 0 -> false
			| _ -> iter (x - 1)
		in iter (int_of_float (sqrt (float_of_int n)))

(* D.2 *)
let rec smallest_prime_factor n = 
	if is_prime n then invalid_arg "invalid arguments"
	else 
		let rec iter x = if n mod x = 0 && is_prime x then x
		else iter (x + 1)
	in iter 2
	 
	
