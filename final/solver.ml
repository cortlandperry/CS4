(* Student Name: Cortland Perry *)
(* CMS Cluster Name: cperry *)

open Binmat

(* Algorithm X:

    If the matrix A has no columns, the current partial solution is a valid
      solution; terminate successfully.

    Otherwise choose a column c (deterministically).  Ideally this should be
    the column with the fewest 1s in it.
   
    Choose a row r such that A(r, c) = 1 (nondeterministically).  If there is no
    such row (i.e. column c is all 0s), the algorithm terminates unsuccessfully.
    Otherwise, include row r in the partial solution.

    Given row r:
      for each column c such that m(r, c) = 1,
        for each row r' such that m(r', c) = 1,
          delete row r from matrix A.
        delete column c from matrix A.

    Repeat this algorithm recursively on the reduced matrix A.
 *)

module AlgorithmX(B : BinaryMatrix) =
  struct
    (* If the algorithm is successful, return a set of integers.
     * Otherwise, return nothing. *)
     
   	(* This function deletes the rows and columns in our matrix *)
	let delete_col_help m cols = 
		let rec iter c m1 = 
			match c with 
			| [] -> m1
			| h :: t -> iter t (B.delete_col m1 h)
		in iter cols m
	
	
	(* Step 4a, deleting the rows, taking as input the row to compare to 
	and matrix m *)
	let delete_row_help m i = 
		let cols = IntSet.of_list (B.cols_for_row m i) in
		let rows = B.rows m in
		let rec iter c r m1 = 
			match r with
			| [] -> m1
			| h :: t -> 
				let h_cols = IntSet.of_list (B.cols_for_row m1 h) in
				if IntSet.is_empty (IntSet.inter (c) (h_cols)) then 
				iter c t m1 else iter c t (B.delete_row m1 h)
		in iter cols rows m
		
	let delete_help m i = 
		let cols = B.cols_for_row m i in
		delete_col_help (delete_row_help m i) cols
	
    (* USE TWO MUTUALLY RECURSIVE FUNCTIONS, TO HAVE IT LOOP AT THE RIGHT TIMES FOR NONE *)
    let rec solve_help m s = 
		if (B.cols m) == [] then Some s
		else 
			let fewest = B.min_col_sum_index m in
			let valid_rows = B.rows_for_col m fewest in
			let rec solve_help2 m1 s1 valid = 
				match valid with
				| [] -> None
				| h :: t -> 
				(* Step 4 *)
				let helper = (solve_help (delete_help m1 h) (IntSet.add h s1)) in
					match helper with
					| None -> solve_help2 m s1 t
					| Some v -> Some v
			in 
			(* need this to check that we have valid rows to start *)
			match valid_rows with
			| [] -> None 
			| _ -> solve_help2 m s valid_rows

(* 
	and solve_help2 m s head tail = 
		match valids with
		| [] -> None
				
	
	else (solve_help (delete_help m row_index) (IntSet.add row_index s))
				
		match c with
		| 0 -> Some s
		| _ -> 
			let fewest = B.min_col_sum_index m in
			let row_index = pick_row m fewest in
			match row_index with
			| -1 -> None
			| _ -> (solve_help (delete_help m row_index) (IntSet.add row_index s))
*)				
		
		
		
    let solve (matrix : B.t) : IntSet.t option = 
		solve_help matrix IntSet.empty 
    
  end

