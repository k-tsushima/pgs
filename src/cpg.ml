open Syntax
open Utils

(* let count_cpg = ref 0 *)

let rec cpg bx ks kv s v env =
  (* count_cpg := !count_cpg + 1; *)
  match bx with
  | Def(name, bx1, bx2) ->
    cpg bx2 ks kv s v ((name, bx1) :: env)
  | Var(name) -> ( 
      try
        let bx = snd (List.find (fun (x, t) -> x = name) env) in
        cpg bx ks kv s v env
      with Not_found -> 
        Printf.printf "%s is not found" name; 
        assert false
    )
  | Skip(h) ->
    if h s = v then
      (ks, kv, s, v)
    else
      assert false
  | Replace ->
    (kv, ks, v, s)
  | Prod(bx1, bx2) ->
    let (ks1, kv1, s1, v1) = cpg bx1 (fun m -> first (ks m)) (fun m -> first (kv m)) (first s) (first v) env in
    let (ks2, kv2, s2, v2) = cpg bx2 (fun m -> second (ks m)) (fun m -> second (kv m)) (second s) (second v) env in
    ((fun x -> Con(ks1 x, ks2 x)), (fun x -> Con(kv1 x, kv2 x)), Con(s1, s2), Con(v1, v2))
  | RearrS(f1, f2, bx) ->
    let (ks, kv, s, v) = cpg bx (fun m -> f1 (ks m)) kv (f1 s) v env in
    ((fun m -> f2 (ks m)), kv, f2 s, v)
  | RearrV(g1, g2, bx) ->
    let (ks, kv, s, v) = cpg bx ks (fun m -> g1 (kv m)) s (g1 v) env in
    (ks, (fun m -> g2 (kv m)), s, g2 v)
  | Case(condsv, conds, bx1, bx2) ->
    if (condsv s v) && (conds s) then
      let (ks, kv, s', v') = cpg bx1 ks kv s v env in 
        if (conds s') && (condsv s v') then
            (ks, kv, s', v')
        else
            assert false
    else
      let (ks, kv, s', v') = cpg bx2 ks kv s v env in 
        if not ((conds s') || (condsv s v')) then
            (ks, kv, s', v')
        else
            assert false
  | Compose(bx1, bx2) ->
    (* let (ks1, kv1, s1, v1) = cpg bx1 ks id s (construct_dummy s) env in *)
    let (ks1, kv1, s1, v1) = cpg bx1 ks id s v env in
    let (ks2, kv2, s2, v2) = cpg bx2 kv1 kv v1 v env in
    ((fun x -> ks1 (ks2 x)), kv2, ks1 s2, v2)
