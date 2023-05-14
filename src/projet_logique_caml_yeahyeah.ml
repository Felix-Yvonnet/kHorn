type var = int

type var_eval =
	| Pos of var
	| Neg of var

type clause = var_eval list

type CNF = int * clause list

let name_of_file = Sys.argv.(1)
in
let text = Arg.read_arg(name_of_file)
in

let rec check_is_khorn_clause nbpos = function
	| [] -> true
	| Neg _ :: q -> check_is_khorn_clause nbpos q
	| Pos _ :: q -> if nbpos >= 1 then false else check_is_khorn_clause (nbpos + 1) q

in

let rec find_shortest_clause length short_c = function
	| [] -> length, short_c
	| new_c::rest_of_cs 
	-> let new_l = List.length new_c in
		let next_l, next_sc =
			if new_l < length
			then new_l, new_c
			else length, short_c
		in find_shortest next_l next_sc rest_of_cs

in

let rec suppr_occ x = function
	
	| [] -> false, []
	
	| (Neg y) :: q
	-> let is_clause_sat, suppr_rec = suppr_occ x
		in
		if x = y 
		then is_clause_sat, suppr_rec
		else is_clause_sat, ((Neg y) :: suppr_rec)
		
	
	| Pos y :: q
	-> let is_clause_sat, suppr_rec = suppr_occ x q
		if y = x
		then true, []
		else is_clause_sat, ((Pos y) :: suppr_rec)
		
in

let rec 
