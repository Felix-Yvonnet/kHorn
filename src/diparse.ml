

let my_assert test message = 
  assert (if not test then print_string message; test)

let start_with text start =
  assert (String.length start = String.length text);
  


let parse = 

  let name_of_file = Sys.argv.(1) in

  let text = Arg.read_arg(name_of_file) in

  my_assert (Array.length text > 0) "The test should start with p cnf <num vars> <num clauses>";
  my_assert text.(0) p cnf 3 2
