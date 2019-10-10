open Syntax
open Bxprog
open Utils
open Minbigul_m

(* test get_m skip1 *)
let get_m_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = get_m skip1 s [] in
  v

(* test get_m replace *)
let get_m_replace = 
  let s = Con(Int 1, Int 2) in
  let v = get_m Replace s [] in
  v

(* test get_m rearrS *)
let get_m_rearrS =
  let s = Int 1 in
  let v = get_m rearrS_d s [] in
  v

(* test get_m rearrV *)
let get_m_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = get_m rearrV_d s [] in
  v

(* test get_m prod *)
let get_m_prod =
  let s = Con(Int 1, Int 2) in
  let v = get_m prod_rs s [] in
  v

(* test get_m phead *)
let get_m_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = get_m phead s [] in
  v

(* test get_m phead2 *)
let get_m_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = get_m phead2 s [] in
  v

(* test get_m replace all *)
let get_m_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = get_m replaceAlldef s [] in
  v

(* test get_m bsnoc *)
let get_m_bsnoc =
  let s = Con(Int 2, (Con(Int 1, Unit))) in
  let v = get_m bsnoc_def s [] in
  v

(* test get_m comp of n replace *)
let get_m_lassoc_comp_replace n =
  let s = Int 1 in
  let v = get_m (lassoc_comp Replace n) s [] in
  v

let get_m_rassoc_comp_replace n =
  let s = Int 1 in
  let v = get_m (rassoc_comp Replace n) s [] in
  v

(* test get_m comp of n phead *)
let get_m_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = get_m (lassoc_comp phead n) s [] in
  v

let get_m_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = get_m (rassoc_comp phead n) s [] in
  v

(* test get_m comp of n phead2 *)
let get_m_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = get_m (lassoc_comp phead2 n) s [] in
  v

let get_m_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = get_m (rassoc_comp phead2 n) s [] in
  v

(* test get_m breverse *)
let get_m_breverse n =
  Hashtbl.reset table_g;
  let s = make_consecutive_list n in
  let v = get_m breverse s [] in  
  v

(* test get_m bmapreplace *)
let get_m_bmapreplace n =
  let s = make_consecutive_list n in
  let v = get_m bmapreplace s [] in  
  v

(* ================================== *)


(* test put_m skip1 *)
let put_m_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let s' = put_m skip1 s v [] in
  s'

(* test put_m replace *)
let put_m_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let s' = put_m Replace s v [] in
  s'

(* test put_m rearrS *)
let put_m_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let s' = put_m rearrS_d s v [] in
  s'

(* test put_m rearrV *)
let put_m_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let s' = put_m rearrV_d s v [] in
  s'

(* test put_m prod *)
let put_m_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let s' = put_m prod_rs s v [] in
  s'

(* test put_m phead *)
let put_m_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let s' = put_m phead s v [] in
  s'

(* test put_m phead2 *)
let put_m_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let s' = put_m phead2 s v [] in
  s'
(* test put_m replace all *)
let put_m_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let s' = put_m replaceAlldef s v [] in
  s'

(* test put_m bsnoc *)
let put_m_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let s' = put_m bsnoc_def s v [] in
  s'

(* test put_m comp of n replace *)
let put_m_lassoc_comp_replace n =
    count_get_m_h := 0;
  let s = Int 1 in
  let v = Int 100 in
  let s' = put_m (lassoc_comp Replace n) s v [] in
  Printf.printf "n : %d - %d\n" (!count_get_m_h) (Hashtbl.length table_g);
  s'

let put_m_rassoc_comp_replace n =
count_get_m_h := 0;
  let s = Int 1 in
  let v = Int 100 in
  let s' = put_m (rassoc_comp Replace n) s v [] in
  Printf.printf "n : %d - %d\n" (!count_get_m_h) (Hashtbl.length table_g);
  s'

(* test put_m comp of n phead *)
let put_m_lassoc_comp_phead n =
count_get_m_h := 0;
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put_m (lassoc_comp phead n) s v [] in
  Printf.printf "n : %d - %d\n" (!count_get_m_h) (Hashtbl.length table_g);
  s'

let put_m_rassoc_comp_phead n =
count_get_m_h := 0;
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let s' = put_m (rassoc_comp phead n) s v [] in
  Printf.printf "n : %d - %d\n" (!count_get_m_h) (Hashtbl.length table_g);
  s'

(* test put_m comp of n phead2 *)
let put_m_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let s' = put_m (lassoc_comp phead2 n) s v [] in
  s'

let put_m_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let s' = put_m (rassoc_comp phead2 n) s v [] in
  s'

(* test put_m breverse *)
let put_m_breverse n =
count_get_m_h := 0;
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let s' = put_m breverse s v [] in
  Printf.printf "n : %d - %d\n" (!count_get_m_h) (Hashtbl.length table_g);
  s'

(* test put_m bmapreplace *)
let put_m_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let s' = put_m bmapreplace s v [] in 
  s'