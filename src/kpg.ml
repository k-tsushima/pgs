open Syntax
open Utils

(* let count_kpg = ref 0  *)

let rec kpg bx ks kv ks' kv' s v env = 
  (* count_kpg := !count_kpg + 1; *)
  match bx with
  | Def(name, bx1, bx2) ->
    kpg bx2 ks kv ks' kv' s v ((name, bx1) :: env)
  | Var(name) -> ( 
      try
        let bx = snd (List.find (fun (x, t) -> x = name) env) in
        kpg bx ks kv ks' kv' s v env
      with Not_found -> 
        Printf.printf "%s is not found" name; 
        assert false
    )
  | Skip(h) ->
    let s = ks' s in
    let v = kv' v in
    let ks' = id in 
    let kv' = id in 
    if h s = v then
      (ks, kv, ks', kv', s, v)
    else
      assert false
  | Replace ->
    (kv, ks, kv', ks', v, s)
  | Prod(bx1, bx2) ->
    let s = ks' s in 
    let v = kv' v in 
    let ks' = id in 
    let kv' = id in 
    let (ks1, kv1, ks1', kv1', s1, v1) = 
      kpg bx1 (fun m -> first (ks m)) (fun m -> first (kv m)) (fun m -> first (ks' m)) (fun m -> first (kv' m)) s v env in
    let (ks2, kv2, ks2', kv2', s2, v2) = 
      kpg bx2 (fun m -> second (ks m)) (fun m -> second (kv m)) (fun m -> second (ks' m)) (fun m -> second (kv' m)) s v env in
    (
      (fun x -> Con(ks1 x, ks2 x)),
      (fun x -> Con(kv1 x, kv2 x)),
      (fun x -> Con(ks1' (first x), ks2' (second x))),
      (fun x -> Con(kv1' (first x), kv2' (second x))),
      Con(s1, s2),
      Con(v1, v2)
    )
  | RearrS(f1, f2, bx) ->
    let (ks, kv, ks', kv', s, v) = kpg bx (fun m -> f1 (ks m)) kv (fun m -> f1 (ks' m)) kv' s v env in
    ((fun m -> f2 (ks m)), kv, (fun m -> f2 (ks' m)), kv', s, v)
  | RearrV(g1, g2, bx) ->
    let (ks, kv, ks', kv', s, v) = kpg bx ks (fun m -> g1 (kv m)) ks' (fun m -> g1 (kv' m)) s v env in
    (ks, (fun m -> g2 (kv m)), ks', (fun m -> g2 (kv' m)), s, v)
  | Case(condsv, conds, bx1, bx2) ->
    let s = ks' s in 
    let v = kv' v in 
    let ks' = id in 
    let kv' = id in 
    if (condsv s v) && (conds s) then
      (* kpg bx1 ks kv ks' kv' s v env  *)
      let (ks, kv, ks', kv', s', v') = kpg bx1 ks kv ks' kv' s v env in 
      let s' = ks' s' in 
      let v' = kv' v' in 
      if (conds s') && (condsv s v') then
        (ks, kv, id, id, s', v')
      else
        assert false
    else
      (* kpg bx2 ks kv ks' kv' s v env *)
      (* let (ks, kv, ks', kv', s', v') = kpg bx2 ks kv ks' kv' s v env in 
         let s' = ks' s' in 
         let v' = kv' v' in 
         if not ((conds s') || (condsv s v')) then
         (ks, kv, id, id, s', v')
         else
         assert false *)
      let (ks, kv, ks', kv', s', v') = kpg bx2 ks kv ks' kv' s v env in
      let v' = kv' v' in
      if not (condsv s v') then
        (ks, kv, (fun s' -> let s' = (ks' s') in if not (conds s') then s' else assert false),
         id, s', v')
      else assert false
  | Compose(bx1, bx2) ->
    (* let (ks1, kv1, ks1', kv1', s1, v1) = kpg bx1 ks id ks' id s (construct_dummy s) env in *)
    let (ks1, kv1, ks1', kv1', s1, v1) = kpg bx1 ks id ks' id s v env in
    let (ks2, kv2, ks2', kv2', s2, v2) = kpg bx2 kv1 kv kv1' kv' v1 v env in
    ((fun x -> ks1 (ks2 x)), kv2, (fun m -> ks1 (ks2' m)), kv2', s2, v2)
