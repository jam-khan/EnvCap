(* utils.mli *)
module type UTILS = sig
    val print_list : int list -> unit
end
  
module Utils : UTILS  (* Expose the Utils module adhering to UTILS *)