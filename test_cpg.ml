let skip1 = Skip (fun _ -> Unit)

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
