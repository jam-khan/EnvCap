(* math_utils.mli *)

(* Module type for utility functions *)
module type UTILS = sig
    val print_list : int list -> unit
  end
  
(* Module type for math operations *)
module type MATH = sig
    val add : int -> int -> int
    val multiply : int -> int -> int
end
  
(* Expose the Utils module adhering to UTILS *)
module Utils : UTILS

(* Expose the Math module adhering to MATH *)
module Math : MATH