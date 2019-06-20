open Syntax

let id x = x

let rec cpg (bx:bigul) (ks:(data -> data)) (kv:(data -> data)) env = 
   match bx with
   | Def(name, bx1, bx2) ->
      cpg bx2 ks kv ((name, bx1) :: env)
   | Var(name) ->
      ( try (
         let bx = snd (List.find (fun (x, t) -> x = name) env) in
            cpg bx ks kv env
         )
         with Not_found -> Printf.printf "%s is not found" name; assert false
      )
   | Skip(h) ->
      if (fun m -> h (ks m)) Unit = (fun m -> (kv m)) Unit
      then (ks, kv)
      else assert false
   | Replace ->
      (kv, ks)
   | RearrS(f1, f2, bx) ->
      let (ks, kv) = cpg bx (fun m -> f1 (ks m)) kv env in
         ((fun m -> f2 (ks m)), kv)
   | RearrV(g1, g2, bx) ->
      let (ks, kv) = cpg bx ks (fun m -> g1 (kv m)) env in
         (ks, (fun m -> g2 (kv m)))
   | Prod(bx1, bx2) ->
      let (ks1, kv1) = cpg bx1 (fun m -> (fun (Con(s1, s2)) -> s1) (ks m)) 
                                 (fun m -> (fun (Con(v1, v2)) -> v1) (kv m))
                                    env in
      let (ks2, kv2) = cpg bx2 (fun m -> (fun (Con(s1, s2)) -> s2) (ks m))
                                 (fun m -> (fun (Con(v1, v2)) -> v2) (kv m)) 
                                    env in
         ((fun x -> Con(ks1 x, ks2 x)),(fun x -> Con(kv1 x, kv2 x)))
   | Compose(bx1, bx2) ->
      (* (ks1, kv1) = cpg bx1 ks ? env *)
      let (ks1, kv1) = cpg bx1 ks id env in
      let (ks2, kv2) = cpg bx2 kv1 kv env in
         ((fun m -> ks1 (ks2 m)), kv2)
   | Case(conds, condsv, bx1, bx2) ->
      if (fun s v -> condsv (ks s) (kv v)) Unit Unit
      then cpg bx1 ks kv env
      else cpg bx2 ks kv env


let skip1 = Skip (fun k -> Unit)

(* test skip1 *)
let bx = skip1 in
let s = Con(Int(1), Int(2)) in
let v = Unit in
let (ks, kv) = cpg bx (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test replace *)
let bx = Replace in
let s = Con(Int(1), Int(2)) in
let v = Con(Int(100), Int(101)) in
let (ks, kv) = cpg bx (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test rearrS *)
let bx = (RearrS((fun m -> Con(m, m)), (fun (Con(m, n)) -> if m = n then m else assert false), Replace)) in
let s = Int(1) in
let v = Con(Int(100), Int(100)) in
let (ks, kv) = cpg bx (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test rearrV *)
let bx = (RearrV((fun m -> Con(m, m)), (fun (Con(m, n)) -> if m = n then m else assert false), Replace)) in
let s = Con(Int(1), Int(1)) in
let v = Int(100) in
let (ks, kv) = cpg bx (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test prod *)
let bx = (Prod(Replace, skip1)) in
let s = Con(Int(1), Int(2)) in
let v = Con(Int(100), Unit) in
let (ks, kv) = cpg bx (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test composition of 2 replace *)
let bx = Compose(Replace, Replace) in
let s = Int(1) in
let v = Int(100) in
let (ks, kv) = cpg bx (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test composition of 3 replace *)
let bx = Compose(Replace, Compose(Replace, Replace)) in
let s = Int(1) in
let v = Int(100) in
let (ks, kv) = cpg bx (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test phead *)
let bx = RearrV((fun v -> Con(v, Unit)), (fun (Con(v, Unit)) -> v), Prod(Replace, skip1)) in
let s = Con(Int(1), Con(Int(2), Unit)) in
let v = Int(100) in
let (ks, kv) = cpg bx (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test compostion of 2 phead *)
(* Compose(bx1, bx2):
      (ks1, kv1) = cpg bx1 ks id env in : Result is CORRECT
      (ks1, kv1) = cpg bx1 ks kv env in : Result is INCORRECT
*)
let phead = RearrV((fun v -> Con(v, Unit)), (fun (Con(v, Unit)) -> v), Prod(Replace, skip1)) in
let s = Con(Con(Int(1), Con(Int(2), Unit)), Con(Con(Int(3),Unit), Unit)) in
let v = Int(100) in
let (ks, kv) = cpg (Compose(phead,phead)) (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test replace all *)
let replaceAllbody1 = RearrS((fun (Con(x, Unit)) -> x), (fun x -> Con(x, Unit)), Replace) in
let replaceAllbody2 = RearrV((fun v -> Con(v, v)), (fun (Con(v1, v2)) -> if v1 = v2 then v1 else assert false), Prod(Replace, Var("replaceAll"))) in
let replaceAll = Case((fun s -> match s with | Con(_, Unit) -> true | _ -> false), (fun s v -> match s with | Con(_, Unit) -> true | _ -> false), replaceAllbody1, replaceAllbody2) in
let replaceAlldef = Def("replaceAll", replaceAll, Var("replaceAll")) in
let s = Con(Int(1), Con(Int(1), Unit)) in
let v = Int(100) in
let (ks, kv) = cpg replaceAlldef (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test bsnoc *)
let bsnoc = Case((fun (Con(_, s)) -> s = Unit),
   (fun s v -> match v with (Con(_, Con(_)))-> false | Con(_, Unit) -> true | Con(_) -> assert false | Dummy -> assert false | Int _ -> assert false | Unit -> assert false | _ -> assert false),

   RearrV((fun (Con(v, Unit)) -> Con(v, Unit)), (fun (Con(v, Unit)) -> (Con(v, Unit))), Replace),
   RearrS((fun (Con(x, Con(y, ys))) -> (Con(y, Con(x, ys)))),
            (fun (Con(y, Con(x, ys))) -> Con(x, Con(y, ys))), Prod(Replace, Var "bsnoc"))) in
let bsnoc_def = Def("bsnoc", bsnoc, Var "bsnoc") in
let s = (Con(Int 1, (Con(Int 2, (Con(Int 3, Unit)))))) in
let v = (Con(Int 5, (Con(Int 6, (Con(Int 7, Unit)))))) in
let (ks, kv) = cpg bsnoc_def (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test breverse *)
let bsnoc = Case((fun (Con(_, s)) -> s = Unit),
   (fun s v -> match v with (Con(_, Con(_)))-> false | Con(_, Unit) -> true | Con(_) -> assert false | Dummy -> assert false | Int _ -> assert false | Unit -> assert false | _ -> assert false),

   RearrV((fun (Con(v, Unit)) -> Con(v, Unit)), (fun (Con(v, Unit)) -> (Con(v, Unit))), Replace),
   RearrS((fun (Con(x, Con(y, ys))) -> (Con(y, Con(x, ys)))),
            (fun (Con(y, Con(x, ys))) -> Con(x, Con(y, ys))), Prod(Replace, Var "bsnoc"))) in
let bfoldr_bsnoc = Case((fun (Con(x, _)) -> x = Unit), (fun (Con(x, _)) _ -> x = Unit), RearrV((fun v -> (Con(Unit, v))), (fun (Con(Unit, v)) -> v), Prod(skip1, Replace)), RearrS((fun (Con(Con(x, xs), e)) -> (Con(x, Con(xs, e)))), (fun (Con(x, Con(xs, e))) -> (Con(Con(x, xs), e))), Compose(Prod(Replace, Var "bfoldr_bsnoc"), Var "bsnoc"))) in
let bfoldr_bsnoc_def = Def("bfoldr_bsnoc", bfoldr_bsnoc,  Def("bsnoc", bsnoc,Var "bfoldr_bsnoc")) in
let breverse = RearrS((fun s -> Con(s, Unit)), (fun (Con(s, Unit)) -> s), bfoldr_bsnoc_def) in
let s = (Con(Int(1), Con(Int(2), Unit))) in
let v = (Con(Int(3), Con(Int(4), Unit))) in
let (ks, kv) = cpg breverse (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

(* test: Replace `Prod` bFoldr bSnoc *)
let bsnoc = Case((fun (Con(_, s)) -> s = Unit),
   (fun s v -> match v with (Con(_, Con(_)))-> false | Con(_, Unit) -> true | Con(_) -> assert false | Dummy -> assert false | Int _ -> assert false | Unit -> assert false | _ -> assert false),

   RearrV((fun (Con(v, Unit)) -> Con(v, Unit)), (fun (Con(v, Unit)) -> (Con(v, Unit))), Replace),
   RearrS((fun (Con(x, Con(y, ys))) -> (Con(y, Con(x, ys)))),
            (fun (Con(y, Con(x, ys))) -> Con(x, Con(y, ys))), Prod(Replace, Var "bsnoc"))) in
let bfoldr_bsnoc = Case((fun (Con(x, _)) -> x = Unit), (fun (Con(x, _)) _ -> x = Unit), RearrV((fun v -> (Con(Unit, v))), (fun (Con(Unit, v)) -> v), Prod(skip1, Replace)), RearrS((fun (Con(Con(x, xs), e)) -> (Con(x, Con(xs, e)))), (fun (Con(x, Con(xs, e))) -> (Con(Con(x, xs), e))), Compose(Prod(Replace, Var "bfoldr_bsnoc"), Var "bsnoc"))) in
let bfoldr_bsnoc_def = Def("bfoldr_bsnoc", bfoldr_bsnoc,  Def("bsnoc", bsnoc,Var "bfoldr_bsnoc")) in
let s = (Con(Int(1), Con(Con(Int(2), Unit), Unit))) in
let v = (Con(Int(3), Con(Int(4), Unit))) in
let (ks, kv) = cpg (Prod(Replace, bfoldr_bsnoc_def)) (fun _ -> s) (fun _ -> v) [] in
   (ks s, kv v)

