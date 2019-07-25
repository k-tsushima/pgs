open Syntax
open Bxprog
open Utils
open Xpg2

(* test xpg2 skip1 *)
let xpg2_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (ks, kv, s', v') = xpg2 skip1 (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test xpg2 replace *)
let xpg2_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (ks, kv, s', v') = xpg2 Replace (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test xpg2 rearrS *)
let xpg2_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (ks, kv, s', v') = xpg2 rearrS_d (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test xpg2 rearrV *)
let xpg2_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (ks, kv, s', v') = xpg2 rearrV_d (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test xpg2 prod *)
let xpg2_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s', v') = xpg2 prod_rs (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test xpg2 phead *)
let xpg2_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (ks, kv, s', v') = xpg2 phead (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test xpg2 phead_with_case *)
let xpg2_phead_with_case =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (ks, kv, s', v') = xpg2 phead_with_case (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test xpg2 phead2 *)
let xpg2_phead2 =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s', v') = xpg2 phead2 (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test xpg2 phead2_with_case *)
let xpg2_phead2_with_case =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s', v') = xpg2 phead2_with_case (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test replace all *)
let xpg2_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (ks, kv, s', v') = xpg2 replaceAlldef (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test bsnoc *)
let xpg2_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (ks, kv, s', v') = xpg2 bsnoc_def (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test comp of n replace *)
let xpg2_lassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (ks, kv, s', v') = xpg2 (lassoc_comp Replace n) (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

let xpg2_rassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (ks, kv, s', v') = xpg2 (rassoc_comp Replace n) (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test comp of n phead *)
let xpg2_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s', v') = xpg2 (lassoc_comp phead n) (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

let xpg2_rassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s', v') = xpg2 (rassoc_comp phead n) (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test comp of n phead_with_case *)
let xpg2_lassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s', v') = xpg2 (lassoc_comp phead_with_case n) (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

let xpg2_rassoc_comp_phead_with_case n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s', v') = xpg2 (rassoc_comp phead_with_case n) (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test comp of n phead2 *)
let xpg2_lassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s', v') = xpg2 (lassoc_comp phead2 n) (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

let xpg2_rassoc_comp_phead2 n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s', v') = xpg2 (rassoc_comp phead2 n) (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test comp of n phead2_with_case*)
let xpg2_lassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s', v') = xpg2 (lassoc_comp phead2_with_case n) (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

let xpg2_rassoc_comp_phead2_with_case n =
  let s = make_consecutive_list n in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s', v') = xpg2 (rassoc_comp phead2_with_case n) (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test breverse *)
let xpg2_breverse_1 =
  let s = (Con(Int 1, Unit)) in
  let v = (Con(Int 3, Unit)) in
  let (ks, kv, s', v') = xpg2 breverse (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test breverse *)
let xpg2_breverse_3 =
  let s = (Con(Int 1, Con(Int 2, Con(Int 3, Unit)))) in
  let v = (Con(Int 4, Con(Int 5, Con(Int 6, Unit)))) in
  let (ks, kv, s', v') = xpg2 breverse (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test breverse *)
let xpg2_breverse_cpg_3 =
  let s = (Con(Int 1, Con(Int 2, Con(Int 3, Unit)))) in
  let v = (Con(Int 4, Con(Int 5, Con(Int 6, Unit)))) in
  let (ks, kv, s', v') = xpg2 breverse (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

let xpg2_breverse n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s', v') = xpg2 breverse (fun _ -> s) (fun _ -> v) s v [] in
  (s', v') 

(* test bmapreplace *)
let xpg2_bmapreplace n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s', v') = xpg2 bmapreplace (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')

(* test bmapreplace_with_case *)
let xpg2_bmapreplace_with_case n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv, s', v') = xpg2 bmapreplace_with_case (fun _ -> s) (fun _ -> v) s v [] in
  (s', v')