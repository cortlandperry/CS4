(* Student Name: Cortland Perry *)
(* CMS Cluster Name: cperry *)

(* Set of integers. *)
module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

(* Map using ints as keys. *)
module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

module type BinaryMatrix =
  sig
    type t

    val make : int -> int -> (int * int) list -> t  

    val nrows : t -> int
    val ncols : t -> int
    val rows  : t -> int list
    val cols  : t -> int list

    val get : t -> int -> int -> bool  

    val dump : t -> unit

    val rows_for_col : t -> int -> int list
    val cols_for_row : t -> int -> int list

    val delete_row : t -> int -> t
    val delete_col : t -> int -> t

    val min_col_sum_index : t -> int  
  end

(*
 * We represent binary matrices as a pair of maps between integer
 * indices and sets of integers.  One map maps row indices to a set of
 * column indices and the other maps column indices to a set of row indices.
 *)

module ImplBinaryMatrix : BinaryMatrix =
  struct
    type t = 
      { 
        _nrows : int; 
        _ncols : int; 

        (* Map between row indices and sets of column indices. *)
        _rows : IntSet.t IntMap.t;

        (* Map between columns indices and sets of row indices. *)
        _cols : IntSet.t IntMap.t;
      }

    let make nrows ncols lst = 
		let rec iter lst rows col = 
			match lst with 
			| _ when nrows < 1 -> invalid_arg "make: nrows must be at least 1"
			| _ when ncols < 1 -> invalid_arg "make: ncols must be at leat 1"
			| [] -> 
				{_nrows = nrows; _ncols = ncols;_rows = rows; _cols = col}
			| (r, c) :: t when r >= nrows || r < 0 || c >= ncols || c < 0
				-> invalid_arg "make: invalid row/column coordinates"
			| (r, c) :: t -> 
				let rowset = IntMap.find_opt r rows in
				let colset = IntMap.find_opt c col in
				match (rowset, colset) with
				| (None, None) -> iter t 
					(IntMap.add r (IntSet.singleton c) rows) 
					(IntMap.add c (IntSet.singleton r) col)
				| (None, Some v) -> iter t 
					(IntMap.add r (IntSet.singleton c) rows) 
					(IntMap.add c (IntSet.add r v) col)
				| (Some v, None) -> iter t 
					(IntMap.add r (IntSet.add c v) rows) 
					(IntMap.add c (IntSet.singleton r) col)
				| (Some v, Some y) -> iter  t 
					(IntMap.add r (IntSet.add c v) rows) 
					(IntMap.add c (IntSet.add r y) col)
		in iter lst IntMap.empty IntMap.empty
    
    let nrows m = m._nrows
    let ncols m = m._ncols

    let rows m = 
		 let binds = (IntMap.bindings m._rows) in
		 List.map fst binds
					
    let cols m = 
		let binds = (IntMap.bindings m._cols) in
		 List.map fst binds

    let get m r c =
      try
        IntSet.mem c (IntMap.find r m._rows)
      with 
        Not_found -> false

    let dump m =
      let dump_map label1 label2 map =
        IntMap.iter (fun k v -> 
            begin
              Printf.printf "%s: %4d; %s: " label1 k label2;
              IntSet.iter (fun e -> Printf.printf "%d " e) v;
              Printf.printf "\n"
            end)
          map
      in
        begin
          Printf.printf "\n-----\n";
          Printf.printf "IMPL_BINARY_MATRIX: nrows = %d; ncols = %d\n"
            m._nrows m._ncols;
          Printf.printf "\nROW -> COLUMN SET MAP:\n";
          dump_map "row" "columns" m._rows;
          Printf.printf "\nCOLUMN -> ROW SET MAP:\n";
          dump_map "column" "rows" m._cols;
          Printf.printf "-----\n\n";
        end
        
    let rows_for_col m c = 
		let index = IntMap.find_opt c m._cols in
			match index with
			| None -> failwith "rows_for_col: column not in _cols map"
			| Some v -> IntSet.elements v
			
			

    let cols_for_row m r = 
		let index = IntMap.find_opt r m._rows in
			match index with
			| None -> failwith "cols_for_row: row not in _rows map"
			| Some v -> IntSet.elements v

    let delete_row m r = 
		let rec iter colmap e = 
			match e with
			| [] ->  colmap
			| h :: t -> 
				let set = IntMap.find h colmap in
				let rem = IntSet.remove r set in
				iter (IntMap.add h rem colmap) t 
		in 
		let index = IntMap.find_opt r m._rows in
		match index with
		| None -> failwith "delete_row: invalid row"
		| Some v -> {_ncols = m._ncols; _nrows = m._nrows; 
			_rows = IntMap.remove r m._rows; 
			_cols = (iter m._cols (cols_for_row m r)) }
			

    let delete_col m c = 
		let rec iter rowmap e = 
			match e with
			| [] ->  rowmap
			| h :: t -> 
				let set = IntMap.find h rowmap in
				let rem = IntSet.remove c set in
				iter (IntMap.add h rem rowmap) t 
		in 
		let index = IntMap.find_opt c m._cols in
		match index with
		| None -> failwith "delete_col: invalid col"
		| Some v -> {_ncols = m._ncols; _nrows = m._nrows; 
			_rows = (iter m._rows (rows_for_col m c)); 
			_cols = IntMap.remove c m._cols}

    let min_col_sum_index m = 
		let binds = IntMap.bindings m._cols in
		let rec iter b mincol minval first =
			match (b, first) with
			| ((c, rs) :: t, true) -> iter t c (IntSet.cardinal rs) false
			| ([], _) -> mincol
			| ((c, rs) :: t, _) when (IntSet.cardinal rs) < minval -> iter t c (IntSet.cardinal rs) false
			| ((c, rs) :: t, _) -> iter t mincol minval false
		in iter binds 0 0 true
    
  end
