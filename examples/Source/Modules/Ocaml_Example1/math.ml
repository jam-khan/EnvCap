open Utils

module type MATH = sig
  val add : int -> int -> int
  val multiply : int -> int -> int
  val print_math_operations : unit -> unit  (* New function that depends on Utils *)
end

(* math.ml *)
module Math : MATH = struct
  (* Use Utils to print intermediate results *)
  let add a b =
    let result = a + b in
    Utils.print_list [a; b; result];  (* Print the inputs and result *)
    result

  let multiply a b =
    let result = a * b in
    Utils.print_list [a; b; result];  (* Print the inputs and result *)
    result

  (* New function that depends on Utils *)
  let print_math_operations () =
    let sum = add 10 20 in
    let product = multiply 5 6 in
    Utils.print_list [sum; product]
end