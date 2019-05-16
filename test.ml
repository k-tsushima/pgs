open Syntax
open Original
open Without_cont
open Cont

(* bx programs *)
let skip1 = Skip (fun k -> Unit)
let phead = RearrV((fun v -> (Con(v, Unit))), (fun (Con(v, Unit)) -> v), Prod(Replace, skip1))

let phead2 = RearrV((fun v -> (Con(v, Unit))), (fun (Con(v, Unit)) -> v), Prod(Replace, Replace))

let replaceAllbody1 = RearrS((fun (Con(x, Unit)) -> x), (fun x -> Con(x, Unit)), Replace)
let replaceAllbody2 = RearrV((fun v -> Con(v, v)), (fun (Con(v1, v2)) -> if v1 = v2 then v1 else assert false), Prod(Replace, Var("replaceAll")))
           
let replaceAll = Case((fun s -> match s with | Con(_, Unit) -> true | _ -> false), (fun s v -> match s with | Con(_, Unit) -> true | _ -> false), replaceAllbody1, replaceAllbody2)

let replaceAlldef = Def("replaceAll", replaceAll, Var("replaceAll"))
           
let rep2 = Prod(Replace, Replace)

let body_phead = Prod(Replace, skip1)

let bsnoc = Case((fun (Con(_, s)) -> s = Unit),
                 (fun s v -> match v with (Con(_, Con(_)))-> false | Con(_, Unit) -> true | Con(_) -> assert false | Dummy -> assert false | Int _ -> assert false | Unit -> assert false | _ -> assert false),
                 
                 RearrV((fun (Con(v, Unit)) -> Con(v, Unit)), (fun (Con(v, Unit)) -> (Con(v, Unit))), Replace),
                 RearrS((fun (Con(x, Con(y, ys))) -> (Con(y, Con(x, ys)))),
                        (fun (Con(y, Con(x, ys))) -> Con(x, Con(y, ys))), Prod(Replace, Var "bsnoc")))

let bsnoc_def = Def("bsnoc", bsnoc, Var "bsnoc")

let bfoldr_bsnoc = Case((fun (Con(x, _)) -> x = Unit), (fun (Con(x, _)) _ -> x = Unit), RearrV((fun v -> (Con(Unit, v))), (fun (Con(Unit, v)) -> v), Prod(skip1, Replace)), RearrS((fun (Con(Con(x, xs), e)) -> (Con(x, Con(xs, e)))), (fun (Con(x, Con(xs, e))) -> (Con(Con(x, xs), e))), Compose(Prod(Replace, Var "bfoldr_bsnoc"), Var "bsnoc")))

let bfoldr_bsnoc_def = Def("bfoldr_bsnoc", bfoldr_bsnoc,  Def("bsnoc", bsnoc,Var "bfoldr_bsnoc"))

let breverse = RearrS((fun s -> Con(s, Unit)), (fun (Con(s, Unit)) -> s), bfoldr_bsnoc_def)

let bfold_snoc_s = Con(Con(Int 1, Con(Int 2, Unit)), Unit)
let bfold_snoc_v = Con(Int 1, Con(Int 2, Unit))

(* s = Con(Con(Int 2, Con(Int 1, Unit)), Unit), v = Con(Int 2, Con(Int 1, Unit))*)

let rec make_composition num = match num with
  | 1 -> replaceAll
  | n -> Compose(replaceAll, make_composition (n - 1))

let rec comp_replaceall num = match num with
  | 1 -> replaceAll
  | n -> Compose(comp_replaceall (n - 1), replaceAll)
       
let rec comp_replace n =
  if n = 0 then Replace
  else Compose(comp_replace (n - 1), Replace)  

let rec comp_reverse n =
  if n = 0 then breverse
  else Compose(comp_reverse (n - 1), breverse)       

(* phead *)
let rec comp_phead n =
  if n = 0
  then phead
  else Compose(comp_phead (n - 1), phead)  
  

                 
(* for cont *)
let rec construct_dummy s = match s with
  | Int(_) | Dummy -> Dummy
  | Unit -> Unit
  | Con(a, b) -> Con(construct_dummy a, construct_dummy b)                 


(* for obtaining source & view *)         
let rec smallest_lst num = match num with
  | 1 -> Con(Int 1, Unit)
  | n -> let m = smallest_lst (num - 1) in
         Con(m, Unit)
                 
let rec make_lst num = match num with
  | 1 -> Con(Int 1, Unit)
  | n -> let m = make_lst (num - 1) in
         Con(Int num, m)

let rec make_num_lst num acc =
  if num = acc then Con(Int num, Unit)
  else Con(Int num, make_num_lst (num + 1) acc)
let make_num_lst num = make_num_lst 0 num
                     
let rec make_num_lst2 num acc =
  if num = acc then Con(Int (num + 105), Unit)
  else Con((Int (num + 105)), make_num_lst2 (num + 1) acc)
let make_num_lst2 num = make_num_lst2 0 num

(* 2 elements list *)
let rec make_lst_bach_san_v2 num = match num with
  | 1 -> Con(Int 1, Con(Int 1, Unit))
  | n -> let m = make_lst_bach_san_v2 (num - 1) in
         Con(m, Con(m, Unit))

(* 3 elements list *)
let rec make_lst_bach_san_v3 num = match num with
  | 1 -> Con(Int 1, (Con(Int 1, Con(Int 1, Unit))))
  | n -> let m = make_lst_bach_san_v3 (num - 1) in
         Con(m, (Con(m, Con(m, Unit))))

(* test for original *)
let test_original bx s v (v', s') _ =
  let s1 = get bx s [] in
  let v1 = put bx s v [] in
  (s1 = s') && (v1 = v')
let auto_test = test_original

let rec deep_lst n =
  if n = 0 then (Int 100)
  else (Con(deep_lst (n - 1), Int n))


(* test for pg *)
let test_without bx s v ans _ =
  (pg bx s v []) = ans
let auto_test = test_without  


(*
(* test for cont *)
let test_cont bx s v ans dummy_fun =
  let ((ks, s), (kv, v)) = kpg bx id id s v [] dummy_fun in
  let s = ks s in
  let v = kv v in
  (s, v) = ans
let auto_test = test_cont

let eval_cont bx s v dummy_fun =
  let ((ks, s), (kv, v)) = kpg bx id id s v [] dummy_fun in
  let s = ks s in
  let v = kv v in
  (s, v)
 *) 

(* change here *)

let dummy_id x = Dummy


let test1 =
  auto_test Replace (Int(3)) (Int(5)) (Int (5), Int (3)) dummy_id

let test2 =
  auto_test (Prod(Replace, Replace)) (Con(Int(2), Int(3))) (Con(Int(5), Int(4))) ((Con(Int(5), Int(4))), (Con(Int(2), Int(3)))) dummy_id

let test3 =
  auto_test (RearrS((fun m -> Con(m, m)), (fun (Con(m, n)) -> if m = n then m else assert false), Replace)) (Int(3)) (Con(Int(4), Int(4))) (Int(4), (Con(Int 3, Int 3))) dummy_id

let test4 =
  auto_test (RearrV((fun m -> Con(m, m)), (fun (Con(m, n)) -> if m = n then m else assert false), Replace)) (Con(Int(4), Int(4))) (Int(3)) ((Con(Int 3, Int 3)), (Int(4))) dummy_id

let test5 =
  auto_test (Compose(Replace, Replace)) (Con(Int(4), Int(4))) (Int(4)) ((Int(4)), (Con(Int(4), Int(4)))) dummy_id
 
let test6 =
  auto_test (Compose(Replace, (Skip (fun m -> Int(3))))) (Con(Int(4), Int(4))) (Int(3)) (Con (Int 4, Int 4), Int 3) dummy_id

let test_skip1 =
  auto_test skip1 (Con(Int(3), Int(10))) Unit ((Con(Int(3), Int(10))), Unit) dummy_id

let test_rep2 =
  auto_test rep2 (Con(Int(3), Int(10))) (Con(Int 1, Unit)) ((Con(Int 1, Unit)), (Con(Int(3), Int(10)))) dummy_id

let test_body_phead =
  auto_test body_phead (Con(Int(3), Int(10))) (Con(Int 1, Unit))  ((Con(Int(1), Int(10))), (Con(Int 3, Unit))) dummy_id
      
let test_case =
  auto_test (Case((fun s -> match s with Con(Con(_, _), _) -> false | _ -> true),
                     (fun s v -> match s with Con(Con(_, _), _) -> false | _ -> true), Replace, Replace)) (Con(Int(3), Int(10))) (Con(Int 1, Unit)) ((Con(Int 1, Unit)), (Con(Int(3), Int(10)))) dummy_id

let test_replaceAll =
  auto_test replaceAlldef (Con(Int 1, (Con(Int 1, Unit)))) (Int 200) (Con (Int 200, Con (Int 200, Unit)), Int 1) dummy_id

let test_replaceAll2 =
  auto_test (Compose(replaceAlldef, replaceAlldef)) (Con((Con(Int 1, Unit)), (Con((Con(Int 1, Unit)), Unit)))) (Int 200) (Con (Con (Int 200, Unit), Con (Con (Int 200, Unit), Unit)), Int 1) dummy_id

let test_reverse =
  auto_test breverse (make_lst 2) (make_lst 2) ((make_lst 2), (make_lst 2)) construct_dummy

(* 10 -> 577 pg. 100 -> 5527, 1000 -> 55027, 10000 -> 550027 *)
(* let test_reverse_g n =
  count_g := 0;
  count_p := 0;
  let s = get (comp_reverse n) (make_lst 2)  [] in
  s
 *)

         
(* 
let test_reverse_p n =
  count_g := 0;
  count_p := 0;
  let s = put (comp_reverse n) (make_lst 2) (make_lst 2) [] in
  s
 *)
  
(*
let rec comp_replace n =
  if n = 0 then Replace
  else Compose(Replace, comp_replace (n - 1)) *)

(* original replace *)
let original_replace_test n =
  count_g := 0;
  count_p := 0;
  count_c := 0;
  let s = put (comp_replace n) (Int 100) (Int 20) [] in
  (s, !count_c, !count_p, !count_g)

let print_num (i1, i2, i3) =
  Printf.printf " c = %d\n p = %d\n g = %d\n" i1 i2 i3;
  flush stdout

(* pg replace *)
let pg_replace_test n =
  Without_cont.count := 0;
  let s = pg (comp_replace n) (Int 100) (Int 20) [] in
  s

(* cont replace *) 
let cont_replace_test n =
  Cont.count := 0;
  let ((ks, s), (kv, v)) = kpg (comp_replace n) id id (Int 100) (Int 20) [] dummy_id in
  let s = ks s in
  let v = kv v in
  (s, v)


(* cont phead *) 
let cont_phead_test n =
  Cont.count := 0;
  let ((ks, s), (kv, v)) = kpg (comp_phead n) id id (smallest_lst (n + 1)) (Int 100) [] dummy_id in
  let s = ks s in
  let v = kv v in
  (s, v)

(* original phead *) 
let original_phead_test n =
  count_g := 0;
  count_p := 0;
  count_c := 0;
  let s = put (comp_phead n) (smallest_lst (n + 1)) (Int 100) [] in
  (*  let v = get (comp_phead n) (smallest_lst (n + 1)) [] in *)
  (s, (), !count_c, !count_p, !count_g)

let original_replaceall_test n =
  count_g := 0;
  count_p := 0;
  count_c := 0;
  let s = put (comp_replaceall n) (smallest_lst (n + 1)) (Int 100) [] in
  let v = get (comp_replaceall n) (smallest_lst (n + 1)) [] in
  (s, v, !count_c, !count_p, !count_g)  

let cont_replaceall_test n =
  let ((ks, s), (kv, v)) = kpg (comp_replaceall n) id id (smallest_lst (n + 1)) (Int 100) [] dummy_id in
  let s = ks s in
  let v = kv v in
  (s, v)
