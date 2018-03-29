(* Student Name: Cortland Perry *)
(* CMS Cluster Name: cperry *)

open Pentomino_types

(*
 * Pieces.
 *)

let make_piece (lbl, ls) =
  { label = lbl; locs = LocSet.of_list ls }

let make_piece_from_string (label, s) =
  let st     = String.trim s in
  let lines  = String.split_on_char '\n' st in
  let coords = ref LocSet.empty in
    begin
      List.iteri
        (fun i s ->
           String.iteri 
             (fun j c -> 
                if c = 'X' then 
                   coords := LocSet.add (i, j) !coords)
             s)
        lines;
      { label; locs = !coords }
    end

let show_piece { label; locs } =
  Printf.printf "LABEL: '%c'; " label;
  Printf.printf "LOCS: ";
  LocSet.iter (fun (i, j) -> Printf.printf "(%d, %d) " i j) locs;
  print_newline ()

let translate_piece { label; locs } drow dcol = 
	let func (x, y) = (drow + x, dcol + y) in
	let newlocs = LocSet.map func locs in
	{ label = label; locs = newlocs }
 
let normalize_piece { label; locs } = 
	let loclist = LocSet.elements locs in
	let rec find_max_x lst move fir = 
		match lst with
		| [] -> move
		| (x, y) :: t when fir = false -> 
			if (x + move) < 0 then find_max_x t (-x) fir 
			else find_max_x t move fir
		| (x, y) :: t -> find_max_x t (-x) (false)
	in let rec find_max_y lst move fir =
		match lst with
		| [] -> move
		| (x, y) :: t when fir = false-> 
			if  (y + move) < 0 then find_max_y t (-y) fir 
			else find_max_y t move fir
		| (x, y) :: t -> find_max_y t (-y) (false)
	in translate_piece {label; locs} 
		(find_max_x loclist 0 true) (find_max_y loclist 0 true)

let reflect_piece { label; locs } = 
	let func (x, y) = (x, -y) in
	let newlocs = LocSet.map func locs in
	normalize_piece { label = label; locs = newlocs } 


let rotate_piece { label; locs } = 
	let neg_row (x, y) = (-x, y) in
	let swap (x, y) = (y, x) in
	let newlocs = LocSet.map swap (LocSet.map neg_row locs) in
	normalize_piece {label = label; locs = newlocs }
	
	
let piece_in_all_orientations p = 
  let p1  = normalize_piece p in
  let p2  = rotate_piece p1 in
  let p3  = rotate_piece p2 in
  let p4  = rotate_piece p3 in
  let p1_ = reflect_piece p1 in
  let p2_ = reflect_piece p2 in
  let p3_ = reflect_piece p3 in
  let p4_ = reflect_piece p4 in
  let piece_list = [p1; p2; p3; p4; p1_; p2_; p3_; p4_] in
    PieceSet.of_list piece_list

let all_normalized_pieces piece_string_list = 
  let all_pieces   = List.map make_piece_from_string piece_string_list in
  let orientations = List.map piece_in_all_orientations all_pieces in
    List.fold_left PieceSet.union PieceSet.empty orientations

(*
 * Boards.
 *)

let on_board p b = 
  let test (row, col) =
    row >= 0 && 
    col >= 0 && 
    row < b.nrows && 
    col < b.ncols &&
    b.member (row, col)
  in
    LocSet.for_all test p.locs

let translate_piece_on_board b p = 
	let rec iter b1 p1 i j result = 
		match i with
		| i1 when i <= b.nrows -> 
			let peace = translate_piece p1 i j in
			if on_board (peace) b1 then
			iter b1 p1 (i + 1) j (PieceSet.add peace result)
			else iter b1 p1 (i + 1) j result
		| _ when j <= b.ncols -> 
			let peace = translate_piece p1 i j in
			if on_board (peace) b1 then
			iter b1 p1 0 (j + 1) (PieceSet.add peace result)
			else iter b1 p1 0 (j + 1) result
		| _ -> result
	in iter b p 0 0 PieceSet.empty
	
		
			 
let all_pieces_on_board piece_string_list b =
  let pieces     = all_normalized_pieces piece_string_list in
  let elems      = PieceSet.elements pieces in
  let translated = List.map (translate_piece_on_board b) elems in
    List.fold_left PieceSet.union PieceSet.empty translated

let make_piece_array piece_string_list b = 
	let set = all_pieces_on_board piece_string_list b in
	let lst = PieceSet.elements set in
	Array.of_list lst

let all_locs_on_board b =
  let rec iter row col lst =
    match () with
      | _ when row = b.nrows -> lst
      | _ when col = b.ncols -> iter (row + 1) 0 lst
      | _ when b.member (row, col) -> iter row (col + 1) ((row, col) :: lst)
      | _ -> iter row (col + 1) lst
  in iter 0 0 []

(*
 * Constraints.
 *)

let make_constraints_array piece_string_list b = 
	let locations = all_locs_on_board b in
	let piecelist = Array.to_list (make_piece_array piece_string_list b) in
	let cast_loc x = Loc x in
	let plocs = Array.of_list (List.map cast_loc locations) in
	let only_label {label; locs } = label in
	let labels = List.map only_label (piecelist) in
	(* I had duplicate labels, thus I will remove them here *)
	let rec remove_dup lst = 
		match lst with
		| [] -> []
		| h :: t -> h :: (remove_dup (List.filter (fun x-> x <> h) t)) in
	let no_dup = remove_dup labels in
	let cast_label l = Label l in
	let plabels = Array.of_list (List.map cast_label no_dup) in
	Array.append plocs plabels
	
	

let make_constraints_map piece_string_list b = 
	let array = make_constraints_array piece_string_list b in
	let rec iter a i r = 
		match i with
		| j when j = ((Array.length a)) -> r
		| _ -> iter a (i + 1) (PconstraintMap.add (Array.get a i) i r)
	in iter array 0 PconstraintMap.empty
		
	


(*
 * Inputs to algorithm X.
 *)

let make_binary_matrix_locs piece_string_list b = 
	let array = make_piece_array piece_string_list b in
	let map = make_constraints_map piece_string_list b in
	let rec iter a rowi result = 
		match rowi with 
		| j when j = (Array.length a) -> result
		| _ -> 
			let p = (Array.get a rowi) in
			let plocations = LocSet.elements p.locs in
			let plabel = p.label in
			(* this gives us an array of constraints *)
			let labelval = (PconstraintMap.find (Label plabel) map) in
			let findingloc x = (PconstraintMap.find (Loc x) map) in
			let locvals = List.map findingloc plocations in
			let allconst = labelval::locvals in
			let make_coord y = (rowi, y) in
			let res = List.map make_coord allconst in
			iter a (rowi + 1) (res @ result)
	in LocSet.of_list (iter array 0 [])
		
	
