module Var: sig
  (* module defining a var with invariants (a variable is an integer strictly positive representing its id) *)
  type t = Vari of int | Top | Bottom
  (* functions to create and get the negation *)
  val to_var: int -> t
  val (~-): t -> t
  val getVar: t -> int
  val is_positive: t -> bool
end = struct
  type t = Vari of int | Top | Bottom
  let to_var = function
    | n when n = min_int -> Bottom
    | n when n = max_int -> Top
    | n -> Vari n

  let (~-) = function
    | Vari var -> Vari(-var)
    | Top -> Bottom
    | Bottom -> Top
  let getVar: t -> int = function
    | Bottom -> min_int
    | Top -> max_int
    | Vari var -> var

  let is_positive: t -> bool = function
    | Vari var when var < 0 -> false
    | _ -> true

end

module Formula = struct
  type t =
      Lit of Var.t
    | Neg of t
    | Conj of t * t
    | Disj of t * t

  let rec size = function
    | Lit v -> 1
    | Neg v -> size v
    | Conj (v1,v2) -> size v1 + size v2
    | Disj (v1,v2) -> size v1 + size v2
  let rec num_positive: t -> int = function
    | Lit v -> if Var.is_positive v then 1 else 0
    | Neg f -> size f - num_positive f
    | Conj (f1,f2) -> num_positive f1 + num_positive f2
    | Disj (f1,f2) -> num_positive f1 + num_positive f2
end

module HNF = struct
  open Formula
  open Var
  type t = int list list

  let rec to_NNF = function
    | Neg (Conj (f1,f2))-> Disj (to_NNF (Neg f1), to_NNF (Neg f2))
    | Neg (Disj (f1,f2))-> Conj (to_NNF (Neg f1), to_NNF (Neg f2))
    | Neg (Lit q) -> Lit (-q)
    | Conj (f1,Lit Bottom) -> Lit Bottom
    | Conj (Lit Bottom, f1) -> Lit Bottom
    | Disj (f1, Lit Bottom) -> to_NNF f1
    | Disj (Lit Bottom, f2) -> to_NNF f2
    | Conj (f1, Lit Top) -> to_NNF f1
    | Conj (Lit Top, f1) -> to_NNF f1
    | Disj (f1, Lit Top) -> Lit Top
    | Disj (Lit Top, f2) -> Lit Top
    | Conj (f1,f2) -> Conj (to_NNF f1, to_NNF f2)
    | Disj (f1,f2) -> Disj (to_NNF f1, to_NNF f2)
    | f -> f

  let rec factorize_formula = function
    | Disj (f1, Conj (f2, f3)) -> Conj (factorize_formula (Disj (f1, f2)), factorize_formula (Disj (f1, f3)))
    | Disj (Conj (f1, f2), f3) -> Conj (factorize_formula (Disj (f1, f2)), factorize_formula (Disj (f1, f3)))
    | Conj (f1, f2) -> Conj (factorize_formula f1, factorize_formula f2)
    | f -> f

  let rec to_list = function
    | Lit q -> [[Lit q]]
    | Neg (Lit q) -> [[Lit (-q)]]
    | Disj (f1, f2) -> [(List.flatten (to_list f1)) @ (List.flatten (to_list f2))]
    | Conj (f1, f2) -> (to_list f1) @ (to_list f2)
      
    | _ -> failwith "Bad argument"

  let rec is_horn: Formula.t list list -> bool = function
    | [] -> true
    | x :: xs -> List.fold_left (fun sum f -> num_positive f + sum) 0 x < 2 && is_horn xs

    let rec pretty_print_ou = function
    | [] -> ()
    | t :: q -> (
      match t with 
        | Lit Bottom -> print_string " bottom "
        | Lit Top -> print_string " top "
        | Lit var -> print_int (getVar var)
        | _ -> failwith "invalid format"
      );
      if q != [] then print_string " \\/ ";
      pretty_print_ou q
  
  let rec pretty_print = function
    | [] -> print_string "\n"
    | t :: q -> 
      print_string "(";
      pretty_print_ou t;
      print_string ")";
      if q != [] then print_string " /\\ ";
      pretty_print q

  let to_HNF formulae = 
    let f = to_list (factorize_formula (to_NNF formulae)) in 
    if is_horn f then f
    else (
      pretty_print f;
      failwith "the formula is not transformable into HNF formula"
    )

end


open Formula
open Var


let formula = Conj (Lit (to_var 1), Disj (Lit (to_var 0), Lit (to_var 2)))

let () = HNF.pretty_print (HNF.to_HNF formula)