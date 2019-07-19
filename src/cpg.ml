open Syntax
open Utils

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
    if h (ks Unit) = kv Unit then
      (ks, kv)
    else
      assert false
  | Replace ->
    (kv, ks)
  | RearrS(f1, f2, bx) ->
    let (ks, kv) = cpg bx (fun m -> f1 (ks m)) kv env in
      ((fun m -> f2 (ks m)), kv)
  | RearrV(g1, g2, bx) ->
    let (ks, kv) = cpg bx ks (fun m -> g1 (kv m)) env in
      (ks, (fun m -> g2 (kv m)))
  | Prod(bx1, bx2) ->
    let (ks1, kv1) = 
      cpg
        bx1
        (fun m -> (fun x -> match x with Con(s1, s2) -> s1 | _ -> assert false) (ks m)) 
        (fun m -> (fun x -> match x with Con(v1, v2) -> v1 | _ -> assert false) (kv m))
        env
    in
    let (ks2, kv2) = 
      cpg 
        bx2
        (fun m -> (fun x -> match x with Con(s1, s2) -> s2 | _ -> assert false) (ks m))
        (fun m -> (fun x -> match x with Con(v1, v2) -> v2 | _ -> assert false) (kv m)) 
        env
    in
      (
        (fun x -> Con(ks1 x, ks2 x)),
        (fun x -> Con(kv1 x, kv2 x))
      )
  | Case(condsv, conds, bx1, bx2) ->
    if (fun s v -> condsv (ks s) (kv v)) Unit Unit then 
      cpg bx1 ks kv env
    else
      cpg bx2 ks kv env
  | Compose(bx1, bx2) ->
    let (ks1, kv1) = cpg bx1 ks (fun x -> removeInt (kv x) x) env in
    let (ks2, kv2) = cpg bx2 kv1 kv env in
    (* ((fun x -> ks1 (ks2 x)), kv2) *)
    ((fun x -> ks1 (getData (kv x) (ks2 x))), kv2)
