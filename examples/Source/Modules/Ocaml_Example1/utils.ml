(* utils.ml *)
module type UTILS = sig
  val print_list : int list -> unit
end

module Utils : UTILS = struct
  let rec iter f = function
    | [] -> ()
    | x :: xs -> f x; iter f xs

  let print_newline () = print_char '\n'

  let print_list lst =
    iter (Printf.printf "%d ") lst;
    print_newline ()
end