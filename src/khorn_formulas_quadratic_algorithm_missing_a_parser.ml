(* We will consider that our terms are integeers *)
type term = int

(* and declare that litteral are terms aiming to have a positive value or a negative one *)
type litteral =
	| Pos of term
	| Neg of term

(* We will implement clauses as lists of litteral *)
type clause = litteral list
(* and conjunctive normal formula as clauses lists, with terms between 1 and n, n integeer *)
type cnf = int * clause list

let name_of_file = Sys.argv.(1)

let text = Arg.read_arg(name_of_file)


let check_is_khorn_clause cl =
in
	(*
	Given a clause [cl],
	[check_is_khorn_clause cl] returns {true} if [cl] is a Khorn clause, that is to say has at most one positive term,
	and {false} otherwise.
	*)
	
	let rec atMostOnePos nbPos = function
		| [] -> true
		| Neg _ :: q -> check_is_khorn_clause nbpos q
		| Pos _ :: q -> if nbPos >= 1 then false else check_is_khorn_clause (nbPos + 1) q
	in atMostOnePos 0 cl

in

let rec check_is_Horn_normal_form = function
	
	(*
	Given a cnf [cl_li],
	[check_is_Horn_normal_form cl_li] returns {true} if [cl_li] is in Horn-normal form,
	and otherwise, prints "Not a Horn clause" then returns {false'}
	*)
	
	| [] -> true
	| cl_i::rest_of_cls
	-> if check_is_khorn_clause cl_i
		then check_is_Horn_normal_form rest_of_cls
		else print_string "Not a Horn clause" ; false

let rec find_shortest_clause min_length_in short_c = function

	(*
	Given an integer [min_length_in], a cnf [short_c] and a cnf list [cl_li],
	[find_shortest_clause min_length_in short_c cl_li] returns one of the cnf in [cl_li] with minimal length and its length,
	under the condition that the latter is inferior to [min_length_in].
	
	When a cnf is found of same length than the current minimal length, it is not chosen,
	unless the said length is 1, and the cnf is of the form [Pos x].
	This ensures that if there are several minimal clauses with only one element,
	the result will be {1, [(Pos x) :: []]}, if there exists such a clause [(Pos x) :: []],
	and '1, {[(Neg x) :: []]} in the other case.
	*)

	| [] -> min_length_in, short_c
	| new_c::rest_of_cs
	-> let new_l = List.length new_c in
		let next_l, next_sc =
			if new_l < min_length_in
			then new_l, new_c
			else
			(
				if min_length_in = 1 && new_l = 1
				then
				(
					match new_c with
						| Pos x :: [] -> 1, new_c
						| Neg x :: [] -> 1, short_c
				)
				else min_length_in, short_c
			)
		in find_shortest next_l next_sc rest_of_cs

in

let rec suppr_occ_local x = function
	(*
	Given a term [x] and a clause [cl],
	[suppr_occ x cl] gets rid of the occurrence of Neg x in cl.
	
	It returns the couple {bool_x, new_cl},
	where bool_x indicates whether the literal Pos x is in cl
	and, if bool_x = false, new_cl corresponds to the changed clause.
	*)
	
	| [] -> false, []
	
	| (Neg y) :: q
	-> let is_clause_sat, suppr_rec = suppr_occ_local x
		in
		if x = y 
		then is_clause_sat, suppr_rec
		else is_clause_sat, ((Neg y) :: suppr_rec)
		
	
	| Pos y :: q
	-> let is_clause_sat, suppr_rec = suppr_occ_local x q
		in
		if y = x
		then true, []
		else is_clause_sat, ((Pos y) :: suppr_rec)
		
in

let rec suppr_occ_global x = function
	(*
	Given a term [x] and a cnf [cl_li],
	[suppr_occ_global x] returns the cnf {cl_li'},
	where all clauses with Pos x are suppressed, and so are all occurrences of Neg x
	*)
	| [] -> []
	
	| h_cl :: t_cls
	-> let is_clause_sat, modified_cl = suppr_occ_local x h_cl
	in
	if is_clause_sat
	then suppr_occ_global x t_cls
	else modified_cl::suppr_occ_global x t_cls

in

let rec pretty_printer_terms = function
	| [] -> print_string "None\n"
	| x::[] -> print_string(string_of_int x); print_string "\n"
	| x::t -> print_string(string_of_int x); print_string " ; "

in

let pos_answer list_of_terms =
	print_string "This formula can be satisfied, with the following terms set at true: " ;
	pretty_printer_terms list_of_pos_terms ;
	print_string "And the rest at false\n"

in

let neg_answer = print_string "This formula cannot be satisfied"

in

let rec solve_khorn_clause_quadra list_of_pos_terms = function
	| [] -> pos_answer list_of_pos_terms
	| h_cl :: t_cls -> let length, sc = find_shortest_clause (List.length h_cl) h_cl li in
		if length = 0 then neg_answer
		else
		(
			if length = 1
			then
			(
				match sc with
				| Pos x :: [] -> solve_khorn_clause_quadra (x::list_of_pos_terms) (suppr_occ_global x t_cls)
				| Neg x :: [] -> pos_answer list_of_pos_terms
				| _ -> failwith "Something went wrong at some point."
			)
			else pos_answer list_of_pos_terms
		)
