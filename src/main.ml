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


let my_assert test message = 
  assert (if not test then (print_string message; print_char '\n'); test)

let find_num_vars_num_clauses text =
  let found = ref false and indice = ref 0 and num_vars = ref 0 and num_clauses = ref 0 and n = String.length text in
  while !indice < n && not !found do
    let new_val = ref (int_of_char text.[!indice] - int_of_char '0') in
    if !new_val>=0 && !new_val<10 then begin
      found := true;
      num_vars := !new_val;
      while !new_val>=0 && !new_val<10 do 
        incr indice;
        new_val := int_of_char text.[!indice] - int_of_char '0';
        num_vars := !num_vars * 10 + !new_val 
      done;
      while !indice < n && text.[!indice] = ' ' do incr indice; done;
      my_assert (!indice<n) "Unexpected end of line, the test should start with \"p cnf <num vars> <num clauses>\"";
      new_val := int_of_char text.[!indice] - int_of_char '0';
      my_assert (!new_val >= 0 && !new_val <= 9) ("Unexpected token " ^ String.make 1 text.[!indice] ^ ", integer required");
      num_clauses := !new_val;
      while !new_val>=0 && !new_val<10 do 
        incr indice;
        new_val := int_of_char text.[!indice] - int_of_char '0';
        num_clauses := !num_clauses * 10 + !new_val 
      done;
    end;
    incr indice
  done;
  my_assert !found "No expression found, the test should start with \"p cnf <num vars> <num clauses>\"";
  !num_vars, !num_clauses

let get_number text indice =
  let op = if text.[!indice] = '-' then (incr indice; (-)) else (+) and n = String.length text and num = ref 0 in
	let old_indice = !indice in
	while !indice < n && text.[!indice] != ' ' do incr indice done;
	for k=old_indice to !indice-1 do 
		let new_val = int_of_char text.[k]  - int_of_char '0' in
		my_assert (new_val >= 0 && new_val <= 9) "Wrong kind of litteral, expected integer";
		num := op (!num * 10) new_val;
	done;
	!num

		
	
	(*let new_val = ref (int_of_char text.[!indice] - int_of_char '0') and num = ref 0 in
  while !new_val>=0 && !new_val<10 do 
    incr indice;
    new_val := int_of_char text.[!indice] - int_of_char '0';
    num := !num * 10 + !new_val 
  done;
  my_assert (text.[!indice] = ' ') "Wrong format var declaration"; 
  op !num*)
  

let parse_disj line =
  let indice = ref 0 and n = String.length line and rez: litteral list ref = ref [] and found = ref false in
  while not !found && !indice < n do
    while line.[!indice] = ' ' do incr indice done;
    let new_var = get_number line indice in 
    if new_var = 0 then found := true
    else rez := (if new_var < 0 then Neg(- new_var) else Pos new_var) :: !rez ;
  done;
  my_assert (!found) "A 0 is expected at the end of the line";
  !rez



let parse = 
	my_assert (Array.length Sys.argv > 0) "Too few arguments given to parse"; 
	let rez:litteral list list ref array = Array.make (Array.length Sys.argv - 1) (ref []) in

	for k=1 to Array.length Sys.argv - 1 do
		let name_of_file = Sys.argv.(k) in

		let text = Arg.read_arg(name_of_file) in

		my_assert (Array.length text > 0) "The file is empty";
		my_assert (String.starts_with ~prefix:"p cnf " text.(0)) "The test should start with \"p cnf <num vars> <num clauses>\", pay attention to the spaces";

		for i = 1 to Array.length text -1 do
			rez.(k-1) := parse_disj text.(i) :: !(rez.(k-1))
		done;
  done;
	rez


let check_is_khorn_clause cl =
	(*
	Given a clause [cl],
	[check_is_khorn_clause cl] returns {true} if [cl] is a Khorn clause, that is to say has at most one positive term,
	and {false} otherwise.
	*)
	
	let rec atMostOnePos nbPos = function
		| [] -> true
		| Neg _ :: q -> atMostOnePos nbPos q
		| Pos _ :: q -> if nbPos >= 1 then false else atMostOnePos (nbPos + 1) q
	in atMostOnePos 0 cl


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
			else false

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
						| _ -> failwith "not supposed to happen"
				)
				else min_length_in, short_c
			)
		in find_shortest_clause next_l next_sc rest_of_cs


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
		-> let is_clause_sat, suppr_rec = suppr_occ_local x q
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
		else modified_cl::(suppr_occ_global x t_cls)


let rec pretty_printer_terms = function
	| [] -> print_string "None\n"
	| x::[] -> print_string(string_of_int x); print_string "\n"
	| x::t -> print_string(string_of_int x); print_string " ; "; pretty_printer_terms t


let pos_answer list_of_terms =
	print_string "This formula can be satisfied, with the following terms set at true: " ;
	pretty_printer_terms list_of_terms ;
	print_string "And the rest at false\n"


let neg_answer () = print_string "This formula cannot be satisfied\n"


let rec pretty_print_list = function
	| [] -> ()
	| x :: xs ->  (match x with | Pos n -> print_int n | Neg n -> print_int (-n)); if xs != [] then  pretty_print_list xs

let rec pretty_print_list_list = function
	| [] -> print_string "\n"
	| x :: xs -> 
		print_string "("; pretty_print_list x; print_string ")"; if xs != [] then print_string ";"; pretty_print_list_list xs

let rec solve_khorn_clause_quadra list_of_pos_terms = function
	| [] -> pos_answer list_of_pos_terms
	| h_cl :: t_cls -> let length, sc = find_shortest_clause (List.length h_cl) h_cl t_cls in
		if length = 0 then neg_answer ()
		else
		(
			if length = 1
			then
			(
				match sc with
				| Pos x :: [] -> solve_khorn_clause_quadra (x::list_of_pos_terms) (suppr_occ_global x (h_cl::t_cls))
				| Neg x :: [] -> pos_answer list_of_pos_terms
				| _ -> failwith "Something went wrong at some point."
			)
			else pos_answer list_of_pos_terms
		)


let () = 
	let formulas = parse in
	for k= 0 to Array.length formulas-1 do
		Printf.printf "%c[33mThe file %s contains the formula:%c[0m" (char_of_int 27) Sys.argv.(k+1) (char_of_int 27) ;
		pretty_print_list_list !(formulas.(k)); print_string "\n";
		if not (check_is_Horn_normal_form !(formulas.(k))) then failwith "not an Horn normal formula";
		solve_khorn_clause_quadra [] !(formulas.(k))
	done;
