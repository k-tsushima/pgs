open Test

  
(* cont *)
let _ =
  let (s', v') = cont_replaceall_test (int_of_string (Sys.argv.(1))) in
(*  print_string (Syntax.print_data s');
  print_newline ();
  print_string (Syntax.print_data v'); *)
  Printf.printf "kpg is called %d\n" (!Cont.count);

       

(* pg *)   
(* let _ =
  let _ =
    pg_phead_test (int_of_string (Sys.argv.(1))) in
    Printf.printf "pg is called %d\n" (!Without_cont.count); *)

       
    
(* original *)
(* let _ =
  let (s, v, i1, i2, i3) = 
    original_replaceall_test (int_of_string (Sys.argv.(1)))  in
  (*  print_string (Syntax.print_data s'); *)
  print_num (i1, i2, i3) *)