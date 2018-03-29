(* search.ml: search strategies *)
(* Student name:                *)
(* CMS cluster login name:      *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)


    let search init = 
		let stack = S.create () in
		let history = [init] in
		S.push history stack;
		let visited = DS.empty in
		let rec iter stk visit =
			match stk with
			| _ when S.is_empty stk -> raise Not_found
			| _ -> 
				let popped = S.pop stk in
				if (DS.mem (List.hd popped) visit) then iter stk visit
				else if D.is_solved (List.hd popped) then popped
				else 
					let children = (D.next (List.hd popped)) in
					let rec iter2 child stk2 vis2 = 
						match child with
						| [] -> iter stk2 vis2
						| h::t ->  
							(S.push (h :: popped) stk2);
							iter2 t stk2 (DS.add (List.hd popped) vis2)
					in iter2 children stk visit
		in iter stack visited 
		
 

    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

