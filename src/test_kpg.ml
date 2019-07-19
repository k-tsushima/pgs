open Syntax
open Bxprog
open Utils
open Kpg

(* test xpg skip1 *)
let kpg_skip1 =
  let s = Con(Int 1, Int 2) in
  let v = Unit in
  let (ks, kv, s, v) = kpg skip1 id id s v [] dummy_id in
    (ks s, kv v)

(* test xpg replace *)
let kpg_replace = 
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 3, Int 4) in
  let (ks, kv, s, v) = kpg Replace id id s v [] dummy_id in
    (ks s, kv v)

(* test xpg rearrS *)
let kpg_rearrS =
  let s = Int 1 in
  let v = Con(Int 100, Int 100) in
  let (ks, kv, s, v) = kpg rearrS_d id id s v [] dummy_id in
    (ks s, kv v)

(* test xpg rearrV *)
let kpg_rearrV =
  let s = Con(Int 1, Int 1) in
  let v = Int 100 in
  let (ks, kv, s, v) = kpg rearrV_d id id s v [] dummy_id in
    (ks s, kv v)

(* test xpg prod *)
let kpg_prod =
  let s = Con(Int 1, Int 2) in
  let v = Con(Int 100, Unit) in
  let (ks, kv, s, v) = kpg prod_rs id id s v [] dummy_id in
    (ks s, kv v)

(* test xpg phead *)
let kpg_phead =
  let s = Con(Int 1, Con(Int 2, Unit)) in
  let v = Int 100 in
  let (ks, kv, s, v) = kpg phead id id s v [] dummy_id in
    (ks s, kv v)

(* test replace all *)
let kpg_replaceall =
  let s = Con(Int 1, Con(Int 1, Unit)) in
  let v = Int 100 in
  let (ks, kv, s, v) = kpg replaceAlldef id id s v [] dummy_id in
    (ks s, kv v)

(* test bsnoc *)
let kpg_bsnoc =
  let s = (Con(Int 2, (Con(Int 1, Unit)))) in
  let v = (Con(Int 3, (Con(Int 4, Unit)))) in
  let (ks, kv, s, v) = kpg bsnoc_def id id s v [] dummy_id in
    (ks s, kv v)

(* test comp of n replace *)
let kpg_lassoc_comp_replace n =
  let s = Int 1 in
  let v = Int 100 in
  let (ks, kv, s, v) = kpg (lassoc_comp Replace n) id id s v [] dummy_id in
   (ks s, kv v)

let kpg_rassoc_comp_replace n =
	let s = Int 1 in
	let v = Int 100 in
	let (ks, kv, s, v) = kpg (rassoc_comp Replace n) id id s v [] dummy_id in
		(ks s, kv v)

(* test comp of n phead *)
let kpg_lassoc_comp_phead n =
  let s = make_smallest_nested_list (n + 1) in
  let v = Int 100 in
  let (ks, kv, s, v) = kpg (lassoc_comp phead n) id id s v [] dummy_id in
   (ks s, kv v)

let kpg_rassoc_comp_phead n =
let s = make_smallest_nested_list (n + 1) in
let v = Int 100 in
let (ks, kv, s, v) = kpg (rassoc_comp phead n) id id s v [] dummy_id in
  (ks s, kv v)
(* 
(* test breverse *)
let kpg_breverse_1 =
  let s = (Con(Int 1, Unit)) in
  let v = (Con(Int 3, Unit)) in
  let (ks, kv, s, v) = kpg breverse id id s v [] dummy_id in
    (ks s, kv v)

(* test breverse *)
let kpg_breverse_2 =
  let s = (Con(Int 1, Con(Int 1, Unit))) in
  let v = (Con(Int 2, Con(Int 2, Unit))) in
  let (ks, kv, s, v) = kpg breverse id id s v [] dummy_id in
    (ks s, kv v) *)
