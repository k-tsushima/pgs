open Syntax

let rec construct_dummy s = match s with
  | Int(_) | Dummy -> Dummy
  | Unit -> Unit
  | Con(a, b) -> Con(construct_dummy a, construct_dummy b)

let count = ref 0
                                       
let rec pg (bx:bigul) s v env =
 (* (print_string "pg is called for ";
   print_string (print bx);
   print_string ".\n s is ";
   print_string (print_data s);
   print_string ".\n v is ";
   print_string (print_data v);
   print_newline ()); *)
  count := !count + 1;
  match bx with
  | Def(name, bx1, bx2) ->
     pg bx2 s v ((name, bx1)::env)
  | Var(name) ->
     (try(let bx = snd (List.find (fun (x, t) -> x = name) env) in
          pg bx s v env)
      with Not_found -> Printf.printf "%s is not found" name; assert false)
  | Skip (h) ->
      (s, if h s = v
          then (* (kv, v) *) v
          else assert false)
  | Replace -> (v, s)
  | Prod(bx1, bx2) ->
     let (s1, v1) = pg bx1 ((fun (Con(s1, s2)) -> s1) s)
                      ((fun (Con(v1, v2)) -> v1) v) env in
     let (s2, v2) = pg bx2 ((fun (Con(s1, s2)) -> s2) s)
                      ((fun (Con(v1, v2)) -> v2) v) env in
     (Con(s1, s2), Con(v1, v2))
  | RearrS(f1, f2, bx) ->
     let (s, v) = pg bx ((fun m -> f1 m) s) v env in
     ((fun m -> f2 m) s, v)
  | RearrV(g1, g2, bx) ->
     let (s, v) = pg bx s ((fun m -> g1 m) v) env in
     (s, (fun m -> g2 m) v)
  | Compose(bx1, bx2) ->
     let (s1, v1) = pg bx1 s (construct_dummy s) env in
     let (s2, v2) = pg bx2 v1 v env in
     let (s3, v3) = pg bx1 s1 s2 env in
     (s3, v2)
  | Case(conds, condsv, bx1, bx2) ->
     if condsv s v
     then pg bx1 s v env
     else pg bx2 s v env

(*  
let test_comp_replace_without n =
  let bx = comp_replace n in
  let (s, v) = pg bx (Int 100) (Int 1) [] in
  (s, v)



let test_pheadn n =
  let (s, v) = pg (make_long_bx n) 
                             (make_lst (n + 1)) (Int 100) [] in
  if v = Int 1 then true else false 
  
let test_bfoldr_bsnoc =
  let (s, v) = pg bfoldr_bsnoc_def bfold_snoc_s bfold_snoc_v [] in
  (s, v) = (Con(Con(Int 2, Con(Int 1, Unit)), Unit), Con(Int 2, Con(Int 1, Unit)))


let test_com_p n =
  let s = pg (comp_replace n) (Int 100) (Int 20) [] in
  s

  

(* 10 -> 577 pg. 100 -> 5527, 1000 -> 55027, 10000 -> 550027 *)
let test_reverse n =
  count := 0;
  let (s, v) = pg (comp_reverse n) (make_lst 100) (make_lst 100) [] in
  ((s, v), !count)
 *)
