open Syntax
open Utils

let count = ref 0

let rec kpg (bx:bigul) (ks:(data->data)) (kv:(data->data)) s v env dummy_fun =
  count := !count + 1; 
  match bx with
  | Def(name, bx1, bx2) ->
    kpg bx2 ks kv s v ((name, bx1) :: env) dummy_fun
  | Var(name) -> (
      try
        let bx = snd (List.find (fun (x, t) -> x = name) env) in
        kpg bx ks kv s v env dummy_fun
      with Not_found -> 
        Printf.printf "%s is not found" name; 
        assert false
    )
  | Skip (h) ->
    let r = ks s in
    let t = kv v in
    if h r = t then
      (id, id, r, t)
    else
      assert false
  | Replace -> 
    (kv, ks, v, s)
  | Prod(bx1, bx2) ->
    let (ks1, kv1, s1, v1) =
      kpg
        bx1 
        (fun m -> (fun x -> match x with Con(s1, s2) -> s1 | _ -> assert false) (ks m))
        (fun m -> (fun x -> match x with Con(v1, v2) -> v1 | Dummy -> Dummy | _ -> assert false) (kv m)) 
        s 
        v 
        env 
        dummy_fun 
    in
    let (ks2, kv2, s2, v2) = 
      kpg 
        bx2 
        (fun m -> (fun x -> match x with Con(s1, s2) -> s2 | _ -> assert false) (ks m))
        (fun m -> (fun x -> match x with Con(v1, v2) -> v2 | Dummy -> Dummy | _ -> assert false) (kv m)) 
        s 
        v 
        env 
        dummy_fun
    in
    (
      (fun x -> match x with Con(s1, s2) -> Con(ks1 s1, ks2 s2) | _ -> assert false),
      (fun x -> match x with Con(v1, v2) -> Con(kv1 v1, kv2 v2) | _ -> assert false), 
      Con(s1, s2),
      Con(v1, v2)
    )
  | RearrS(f1, f2, bx) ->
    let (ks, kv, s, v) = kpg bx (fun m -> f1 (ks m)) kv s v env dummy_fun in
    ((fun m -> f2 (ks m)), kv, s, v)
  | RearrV(g1, g2, bx) ->
    let (ks, kv, s, v) = kpg bx ks (fun m -> g1 (kv m)) s v env dummy_fun in
    (ks, (fun m -> g2 (kv m)), s, v)
  | Case(condsv, conds, bx1, bx2) ->
    let (ks, s) = (id, ks s) in
    let (kv, v) = (id, kv v) in
    (if (condsv s v)
     then kpg bx1 ks kv s v env dummy_fun
     else kpg bx2 ks kv s v env dummy_fun)
  | Compose(bx1, bx2) ->
    let (ks1, kv1, s1, v1) = kpg bx1 ks id s (dummy_fun s) env dummy_fun in
    let (ks2, kv2, s2, v2) = kpg bx2 kv1 kv v1 v env dummy_fun in
    ((fun m -> replace_dummy (ks1 s1) (ks2 m)), kv2, s2, v2)
