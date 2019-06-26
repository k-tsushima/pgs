(* open Syntax *)

let id x = x

let rec removeInt dat (x:data) = 
   match dat with
   | Con(a, b) -> Con(removeInt(a) x, removeInt(b) x)
   | Int(y) -> x
   | Unit -> Unit
   | Dummy -> Dummy

let rec getData dat (x:data) = 
   match (dat,x) with 
   | (Unit, z) -> None
   | (Int(y),z) -> Some(z)
   | (Con(a, b), Con(c,d)) -> 
      let a' = getData(a) c in
      let b' = getData(b) d in
         (match (a',b') with
         | (None, None) -> None
         | (None, Some(y)) -> Some(y)
         | (Some(y), None) -> Some(y)
         | (Some(y1), Some(y2)) -> if y1 = y2 then Some(y1) else assert false
         )
   | _ -> assert false

let getData dat (x:data) =
   let dat' = getData dat x in
      match dat' with
      | None -> assert false
      | Some(y) -> y

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
      let (ks1, kv1) = cpg bx1 ks (fun x -> removeInt (kv x) x) env in
      let (ks2, kv2) = cpg bx2 kv1 kv env in
         (
            (fun x -> ks1 ( 
               getData (kv x) (ks2 x)
            ))
            , kv2)
   | Case(conds, condsv, bx1, bx2) ->
      if (fun s v -> condsv (ks s) (kv v)) Unit Unit
      then cpg bx1 ks kv env
      else cpg bx2 ks kv env
