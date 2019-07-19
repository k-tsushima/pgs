open Syntax
open Bxprog
open Utils
open Cpg

(* test cpg skip1 *)
let cpg_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (ks, kv) = cpg skip1 (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)

(* test cpg replace *)
let cpg_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (ks, kv) = cpg Replace (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)

(* test cpg rearrS *)
let cpg_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (ks, kv) = cpg rearrS_d (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)

(* test cpg rearrV *)
let cpg_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (ks, kv) = cpg rearrV_d (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)

(* test cpg prod *)
let cpg_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (ks, kv) = cpg prod_rs (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)

(* test cpg phead *)
let cpg_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (ks, kv) = cpg phead (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)

(* test cpg replaceAll *)
let cpg_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (ks, kv) = cpg replaceAlldef (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)

(* test cpg bsnoc *)
let cpg_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (ks, kv) = cpg bsnoc_def (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)

(* test cpg comp of n phead *)
let cpg_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv) = cpg (lassoc_comp phead n) (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

let cpg_rassoc_comp_phead n =
let s = make_smallest_nested_list (n + 1) in
let v = Int 100 in
let (ks, kv) = cpg (rassoc_comp phead n) (fun _ -> s) (fun _ -> v) [] in
  (ks s, kv v)

(* test cpg breverse *)
let cpg_breverse_1 =
  let s = (Con(Int 1, Unit)) in
  let v = (Con(Int(3), Unit)) in
  let (ks, kv) = cpg breverse (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)

let cpg_breverse_2 =
  let s = (Con(Int 1, Con(Int 1, Unit))) in
  let v = (Con(Int 2, Con(Int 2, Unit))) in
  let (ks, kv) = cpg breverse (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)

let cpg_breverse n =
  let s = make_same_elements_list 1 n in
  let v = make_same_elements_list 2 n in
  let (ks, kv) = cpg breverse (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)

let cpg_breverse_0 n =
  let s = make_consecutive_list n in
  let v = make_consecutive_list n in
  let (ks, kv) = cpg breverse (fun _ -> s) (fun _ -> v) [] in
    (ks s, kv v)
