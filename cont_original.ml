type data = Con of data * data
          | Int of int
          | Dummy
          | Unit

type bigul = Skip of (data -> data) 
           | Replace
           | Prod of bigul * bigul
           | RearrS of (data -> data) * (data -> data) * bigul
           | RearrV of (data -> data) * (data -> data) * bigul
           | Compose of bigul * bigul
           | Case of (data -> data -> bool) * bigul * bigul
           | Var of string
           | Def of string * bigul * bigul

let debug_flg = true

let skip1 = Skip (fun k -> Unit)
let phead = RearrV((fun v -> (Con(v, Unit))), (fun (Con(v, Unit)) -> v), Prod(Replace, skip1))

let phead2 = RearrV((fun v -> (Con(v, Unit))), (fun (Con(v, Unit)) -> v), Prod(Replace, Replace))

let replaceAllbody1 = RearrS((fun (Con(x, Unit)) -> x), (fun x -> Con(x, Unit)), Replace)
let replaceAllbody2 = RearrV((fun v -> Con(v, v)), (fun (Con(v1, v2)) -> if v1 = v2 then v1 else assert false), Prod(Replace, Var("replaceAll")))
           
let replaceAll = Case((fun s v -> match s with | Con(_, Unit) -> true | _ -> false), replaceAllbody1, replaceAllbody2)

let replaceAlldef = Def("replaceAll", replaceAll, Var("replaceAll"))
           
let rep2 = Prod(Replace, Replace)

let body_phead = Prod(Replace, skip1)

let bsnoc = Case((fun s v -> match s with Con(_, Unit) -> true | _ -> false), RearrV((fun (Con(v, Unit)) -> Con(v, Unit)), (fun (Con(v, Unit)) -> (Con(v, Unit))), Replace), RearrS((fun (Con(x, Con(y, ys))) -> (Con(y, Con(x, ys)))), (fun (Con(y, Con(x, ys))) -> Con(x, Con(y, ys))), Prod(Replace, Var "bsnoc")))

let bsnoc_def = Def("bsnoc", bsnoc, Var "bsnoc")

let bfoldr_bsnoc = Case((fun (Con(x, _)) _ -> x = Unit), RearrV((fun v -> (Con(Unit, v))), (fun (Con(Unit, v)) -> v), Prod(skip1, Replace)), RearrS((fun (Con(Con(x, xs), e)) -> (Con(x, Con(xs, e)))), (fun (Con(x, Con(xs, e))) -> (Con(Con(x, xs), e))), Compose(Prod(Replace, Var "bfoldr_bsnoc"), Var "bsnoc")))
                
let id x = x

let rec dummy_replace data x = match data with
  | Dummy -> x
  | Con(d1, d2) -> Con(dummy_replace d1 x, dummy_replace d2 x)
  | Int(_) | Unit -> data

let print bx = match bx with
  | Def(_) -> "def"
  | Var(_) -> "var"
  | Skip(_) -> "skip"
  | Replace -> "replace"
  | Prod(_) -> "prod"
  | RearrS(_) -> "rearrS"
  | RearrV(_) -> "rearrV"
  | Compose(_) -> "compose"
  | Case(_) -> "case"
                   
(* *)
let rec kpg (bx:bigul) (ks:(data->data)) (kv:(data->data)) s v env =
  if debug_flg
  then 
  (print_string "kpg is called for ";
   print_string (print bx);
   print_newline ());
  match bx with
  | Def(name, bx1, bx2) ->
     kpg bx2 ks kv s v ((name, bx1) :: env)
  | Var(name) ->
     (try(let bx = snd (List.find (fun (x, t) -> x = name) env) in
          kpg bx ks kv s v env)
      with Not_found -> Printf.printf "%s is not found" name; raise Not_found)
  | Skip (h) ->
     let r = ks s in
     ((id, r), let t = kv v in if h r = t
                               then (* (kv, v) *) (id, t)
                               else assert false)
  | Replace -> 
     ((kv, v), (ks, s))
  | Prod(bx1, bx2) ->
     let ((ks1, s1), (kv1, v1)) = kpg bx1 (fun m ->(fun (Con(s1, s2)) -> s1) (ks m)) (fun m -> (fun (Con(v1, v2)) -> v1) (kv m)) s v env in
     let ((ks2, s2), (kv2, v2)) = kpg bx2 (fun m ->(fun (Con(s1, s2)) -> s2) (ks m)) (fun m -> (fun (Con(v1, v2)) -> v2) (kv m)) s v env in
     (((fun (Con(s1, s2)) -> Con(ks1 s1, ks2 s2)), Con(s1, s2)), ((fun (Con(v1, v2)) -> Con(kv1 v1, kv2 v2)), Con(v1, v2)))
  | RearrS(f1, f2, bx) ->
     let ((ks, s), (kv, v)) = kpg bx (fun m -> f1 (ks m)) kv s v env in
     (((fun m -> f2 (ks m)), s), (kv, v))

  | RearrV(g1, g2, bx) ->
     let ((ks, s), (kv, v)) = kpg bx ks (fun m -> g1 (kv m)) s v env in
     ((ks, s), ((fun m -> g2 (kv m)), v))
  | Compose(bx1, bx2) ->
     let ((ks1, s1), (kv1, v1)) = kpg bx1 ks id s Dummy env in
     let ((ks2, s2), (kv2, v2)) = kpg bx2 kv1 kv v1 v env in
     (((fun m -> dummy_replace (ks1 s1) (ks2 m)), s2), (kv2, v2))
  | Case(condf, bx1, bx2) ->
     let (ks, s) = (id, ks s) in
     let (kv, v) = (id, kv v) in
     (if (condf s v)
      then kpg bx1 ks kv s v env
      else kpg bx2 ks kv s v env)
     

let rec deep_lst n =
  if n = 0 then (Int 100)
  else (Con(deep_lst (n - 1), Int n))

let rec make_long_bx n =
  if n = 0
  then phead
  else Compose(phead, make_long_bx (n - 1))

let rec make_lst num = match num with
  | 1 -> Con(Int 1, Unit)
  | n -> let m = make_lst (num - 1) in
         Con(m, Unit)

let rec make_num_lst num acc =
  if num = acc then Con(Int num, Unit)
  else Con(Int num, make_num_lst (num + 1) acc)
let make_num_lst num = make_num_lst 0 num

let rec make_num_lst2 num acc =
  if num = acc then Con(Int num, Unit)
  else Con((Int (num + 105)), make_num_lst2 (num + 1) acc)
let make_num_lst2 num = make_num_lst2 0 num

(* 2 elements *)
let rec make_lst_bach_san_v2 num = match num with
  | 1 -> Con(Int 1, Con(Int 1, Unit))
  | n -> let m = make_lst_bach_san_v2 (num - 1) in
         Con(m, Con(m, Unit))

let rec make_lst_bach_san_v3 num = match num with
  | 1 -> Con(Int 1, (Con(Int 1, Con(Int 1, Unit))))
  | n -> let m = make_lst_bach_san_v3 (num - 1) in
         Con(m, (Con(m, Con(m, Unit))))

let rec make_composition num = match num with
  | 1 -> replaceAll
  | n -> Compose(replaceAll, make_composition (n - 1))
         
let test_replaceAll num =
  let ((ks, s), (kv, v)) = kpg (make_composition num) id id (make_lst num) (Int 200) [] in
  let s = ks s in
  let v = kv v in
  (s, v) 

(* Example: 3 composition of replaceAll -> replaceAll では s1 等を使う必要がない *)
let ((ks1, s1), (kv1, v1)) = kpg replaceAlldef id id (make_lst 3) Dummy [];;
let ((ks2, s2), (kv2, v2)) = kpg replaceAlldef kv1 id v1 Dummy [];;
let ((ks3, s3), (kv3, v3)) = kpg replaceAlldef kv2 id v2 (Int 100) [];;

let ((ks1, s1), (kv1, v1)) = kpg (make_composition 3) id id (make_lst 3) (Int 100) [];;

(* Example: 2 composition of phead -> これを考える必要あり *)
let ((ks1, s1), (kv1, v1)) = kpg phead id id (Con((Con(Int 1, Con(Int 2, Int 9))), (Con(Int 3, (Con(Int 4, Int 5)))))) Dummy [];;
let ((ks2, s2), (kv2, v2)) = kpg phead kv1 id v1 (Int 100) []

let test_bsnoc1 =
  let ((ks, s), (kv, v)) = kpg bsnoc_def id id (Con(Int 100, (make_num_lst2 10))) (make_num_lst 10) [] in
  ((ks, s), (kv, v))
                         
let test_bsnoc2 =
  let ((ks, s), (kv, v)) = kpg bsnoc_def id id (Con(Int 2, (Con(Int 1, Unit)))) (Int 100) [] in
  ((ks, s), (kv, v))
                         
let test_phead2 =
  let ((ks, s), (kv, v)) = kpg (Compose(phead, phead)) id id
                             (Con((Con(Int 1, Con(Int 2, Int 9))), (Con(Int 3, (Con(Int 4, Int 5)))))) (Int 100) [] in
  let s = ks s in
  let v = kv v in
  (s, v) = (Con((Con(Int 100, Con(Int 2, Int 9))), (Con(Int 3, (Con(Int 4, Int 5))))), Int 1)
                           
                           
  
let lst = deep_lst 2
let longbx = make_long_bx 1

               
(* 

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
