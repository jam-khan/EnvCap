(* math.mli *)
module type MATH = sig
    val add : int -> int -> int
    val multiply : int -> int -> int
    val print_math_operations : unit -> unit  (* New function that depends on Utils *)
end
  
module Math : MATH  (* Expose the Math module adhering to MATH *)