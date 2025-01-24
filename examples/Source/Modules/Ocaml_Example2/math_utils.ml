(* math_utils.ml *)

(* Module type for utility functions *)
module type UTILS = sig
  val print_list : int list -> unit
end

(* Module type for math operations *)
module type MATH = sig
  val add : int -> int -> int
  val multiply : int -> int -> int
end

(* Implement the Utils module *)
module Utils : UTILS = struct
  let rec iter f = function
    | [] -> ()
    | x :: xs -> f x; iter f xs

  let print_newline () = print_char '\n'

  let print_list lst =
    iter (Printf.printf "%d ") lst;
    print_newline ()
end

(* Implement the Math module, which depends on Utils *)
module Math : MATH = struct
  let add a b =
    let result = a + b in
    Utils.print_list [a; b; result];  (* Use Utils to print intermediate results *)
    result

  let multiply a b =
    let result = a * b in
    Utils.print_list [a; b; result];  (* Use Utils to print intermediate results *)
    result
end