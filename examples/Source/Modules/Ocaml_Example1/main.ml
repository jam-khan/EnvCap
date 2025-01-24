(* main.ml *)

(* Open the Utils and Math modules to bring their contents into scope *)
open Utils
open Math

module type MAKEPROGRAM = sig
  val run : unit -> unit
end
(* Define a functor that takes modules as arguments *)
module MakeProgram (U : UTILS) (M : MATH) : MAKEPROGRAM = struct
  let run () =
    let lst = [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5] in
    print_string "Original list: ";
    U.print_list lst;

    (* Use the Math module to perform operations *)
    let sum = M.add 10 20 in
    let product = M.multiply 5 6 in

    print_string "Sum of 10 and 20: ";
    print_int sum;
    print_newline ();

    print_string "Product of 5 and 6: ";
    print_int product;
    print_newline ();

    (* Call the new function that depends on Utils *)
    M.print_math_operations ();

    (* Sort the list using a custom function *)
    let sorted_lst = List.sort compare lst in
    print_string "Sorted list: ";
    U.print_list sorted_lst
end

(* Instantiate the functor with the Utils and Math modules *)
module Program = MakeProgram (Utils) (Math)

(* Run the program *)
let () = Program.run ()