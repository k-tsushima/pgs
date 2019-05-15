open Syntax

let id x = x

let rec dummy_replace data x = match data with
  | Dummy -> x
  | Con(d1, d2) -> Con(dummy_replace d1 x, dummy_replace d2 x)
  | Int(_) | Unit -> data

let count = ref 0


  
(* *)
let rec kpg (bx:bigul) (ks:(data->data)) (kv:(data->data)) s v env dummy_fun =
  count := !count + 1; 
  if debug_flg
  then 
  (print_string "kpg is called for ";
   print_string (print bx);
   print_string ".\n s is ";
   print_string (print_data s);
   print_string ".\n v is ";
   print_string (print_data v);
   print_newline ());
  match bx with
  | Def(name, bx1, bx2) ->
     kpg bx2 ks kv s v ((name, bx1) :: env) dummy_fun
  | Var(name) ->
     (try(let bx = snd (List.find (fun (x, t) -> x = name) env) in
          kpg bx ks kv s v env dummy_fun)
      with Not_found -> Printf.printf "%s is not found" name; assert false)
  | Skip (h) ->
     let r = ks s in
     ((id, r)(* (ks, s) *), let t = kv v in if h r = t
                               then (* (kv, v) *) (id, t)
                               else assert false)
  | Replace -> 
     ((kv, v), (ks, s))
  | Prod(bx1, bx2) ->
     let ((ks1, s1), (kv1, v1)) = kpg bx1 (fun m ->(fun (Con(s1, s2)) -> s1) (ks m))
                                    (fun m -> (function (Con(v1, v2)) -> v1 | Dummy -> Dummy) (kv m)) s v env dummy_fun in
     let ((ks2, s2), (kv2, v2)) = kpg bx2 (fun m ->(fun (Con(s1, s2)) -> s2) (ks m))
                                    (fun m -> (function (Con(v1, v2)) -> v2 | Dummy -> Dummy) (kv m)) s v env dummy_fun in
     (((fun (Con(s1, s2)) -> Con(ks1 s1, ks2 s2)), (Con(s1, s2))),
      ((fun (Con(v1, v2)) -> Con(kv1 v1, kv2 v2)), (Con(v1, v2))))
  | RearrS(f1, f2, bx) ->
     let ((ks, s), (kv, v)) = kpg bx (fun m -> f1 (ks m)) kv s v env dummy_fun in
     (((fun m -> f2 (ks m)), s), (kv, v))

  | RearrV(g1, g2, bx) ->
     let ((ks, s), (kv, v)) = kpg bx ks (fun m -> g1 (kv m)) s v env dummy_fun in
     ((ks, s), ((fun m -> g2 (kv m)), v))
  | Compose(bx1, bx2) ->
     let ((ks1, s1), (kv1, v1)) = kpg bx1 ks id s (dummy_fun s) env dummy_fun in
     let ((ks2, s2), (kv2, v2)) = kpg bx2 kv1 kv v1 v env dummy_fun in
     (((fun m -> dummy_replace (ks1 s1) (ks2 m)), s2), (kv2, v2))
  | Case(conds, condsv, bx1, bx2) ->
     let (ks, s) = (id, ks s) in
     let (kv, v) = (id, kv v) in
     (if (condsv s v)
      then kpg bx1 ks kv s v env dummy_fun
      else kpg bx2 ks kv s v env dummy_fun)
     
(*
let test_pheadn n =
  let ((ks, s), (kv, v)) = kpg (make_long_bx n) id id
                             (make_lst (n + 1)) (Int 100) [] in
  let s = ks s in
  let v = kv v in
  if v = Int 1 then true else false 
       




         
let test_replaceAll num =
  let ((ks, s), (kv, v)) = kpg (make_composition num) id id (make_lst num) (Int 200) [] in
  let s = ks s in
  let v = kv v in
  (s, v)

(* current test 
let ((ks, s), (kv, v)) = kpg bfoldr_bsnoc_def id id bfold_snoc_s bfold_snoc_v [];;    *)

let test_replace n =
  count := 0;
  let ((ks, s), (kv, v)) = kpg (comp_replace n) id id (Int 100) (Int 2) [] in
  ((ks s, kv v), !count)  

let test_reverse n =
  count := 0;
  let ((ks, s), (kv, v)) = kpg (comp_reverse n) id id (make_lst 100) (make_lst 100) [] in
  ((ks s, kv v), !count)

  (*

(* Example: 3 composition of replaceAll -> replaceAll では s1 等を使う必要がない *)
let ((ks1, s1), (kv1, v1)) = kpg replaceAlldef id id (make_lst 3) Dummy [];;
let ((ks2, s2), (kv2, v2)) = kpg replaceAlldef kv1 id v1 Dummy [];;
let ((ks3, s3), (kv3, v3)) = kpg replaceAlldef kv2 id v2 (Int 100) [];;

let ((ks1, s1), (kv1, v1)) = kpg (make_composition 3) id id (make_lst 3) (Int 100) [];;

(* Example: 2 composition of phead -> これを考える必要あり *)
let ((ks1, s1), (kv1, v1)) = kpg phead id id (Con((Con(Int 1, Con(Int 2, Int 9))), (Con(Int 3, (Con(Int 4, Int 5)))))) Dummy [];;
let ((ks2, s2), (kv2, v2)) = kpg phead kv1 id v1 (Int 100) []


let test_bsnoc1 =
  let ((ks, s), (kv, v)) = kpg bsnoc_def id id (make_num_lst2 2) (make_num_lst 2) [] in
  ((ks, s), (kv, v))

let test_bsnoc1 =
  let ((ks, s), (kv, v)) = kpg bsnoc_def id id (make_num_lst2 10) (make_num_lst 10) [] in
  ((ks, s), (kv, v))

let ((ks, s), (kv, v)) = kpg breverse id id (make_num_lst 1) (make_num_lst2 1) []
  
let test_breverse =
  let ((ks, s), (kv, v)) = kpg breverse id id (make_num_lst 2) (make_num_lst2 2) [] in
  ((ks, s), (kv, v))  

  
let test_breverse =
  let ((ks, s), (kv, v)) = kpg breverse id id (make_num_lst 10) (make_num_lst 10) [] in
  ((ks, s), (kv, v))  
                         
let test_bsnoc2 =
  let ((ks, s), (kv, v)) = kpg bsnoc_def id id (Con(Int 1, (Con(Int 2, (Con(Int 3, Unit)))))) (Con(Int 5, (Con(Int 6, (Con(Int 7, Unit)))))) [] in
  ((ks, s), (kv, v))
                         
let test_phead2 =
  let ((ks, s), (kv, v)) = kpg (Compose(phead, phead)) id id
                             (Con((Con(Int 1, Con(Int 2, Int 9))), (Con(Int 3, (Con(Int 4, Int 5)))))) (Int 100) [] in
  let s = ks s in
  let v = kv v in
  (s, v) = (Con((Con(Int 100, Con(Int 2, Int 9))), (Con(Int 3, (Con(Int 4, Int 5))))), Int 1)
                           
                           
  
let lst = deep_lst 2
let longbx = make_long_bx 1

               


10000 -> 3
15000 -> 5
20000 -> 18

let lst = deep_lst 25000
let longbx = make_long_bx 24900

let test_longbx =
  let ((ks, s), (kv, v)) = kpg longbx id id lst (Int(200)) in
  let s = ks s in
  let v = kv v in
  (s, v)

 *)
 *)
