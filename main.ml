open Test

let _ =
  let _ = cont_replace_test (int_of_string (Sys.argv.(1))) in
  Printf.printf "kpg is called %d\n" (!Cont.count);

       
(* 
(* pg *)   
let _ =
  let _ =
    pg_replace_test (int_of_string (Sys.argv.(1))) in
    Printf.printf "pg is called %d\n" (!Without_cont.count);
 *)
       
(*    
(* original *)
let _ =
  let (_, i1, i2, i3) = 
    original_replace_test (int_of_string (Sys.argv.(1)))  in
  print_num (i1, i2, i3)
 *)
