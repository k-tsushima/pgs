open Syntax
open Utils

let count_pg = ref 0

let rec pg (bx:bigul) s v env =
  count_pg := !count_pg + 1;
  match bx with
  | Def(name, bx1, bx2) ->
    pg bx2 s v ((name, bx1)::env)
  | Var(name) -> (
      try
        let bx = snd (List.find (fun (x, t) -> x = name) env) in
        pg bx s v env
      with Not_found -> 
        Printf.printf "%s is not found" name; 
        assert false
    )
  | Skip (h) ->
    (s, if h s = v then v else assert false)
  | Replace ->
    (v, s)
  | Prod(bx1, bx2) ->
    let (s1, v1) = pg bx1 (first s) (first v) env in
    let (s2, v2) = pg bx2 (second s) (second v) env in
    (Con(s1, s2), Con(v1, v2))
  | RearrS(f1, f2, bx) ->
    let (s, v) = pg bx ((fun m -> f1 m) s) v env in
    ((fun m -> f2 m) s, v)
  | RearrV(g1, g2, bx) ->
    let (s, v) = pg bx s ((fun m -> g1 m) v) env in
    (s, (fun m -> g2 m) v)
  | Case(condsv, conds, bx1, bx2) ->
    if (condsv s v) && (conds s) then
        let (s', v') = pg bx1 s v env in 
            if (conds s') && (condsv s v') then
                (s', v')
            else
                assert false
    else
        let (s', v') = pg bx2 s v env in 
            if not (conds s' || condsv s v') then
                (s', v')
            else
                assert false
  | Compose(bx1, bx2) ->
    let (s1, v1) = pg bx1 s (construct_dummy s) env in
    let (s2, v2) = pg bx2 v1 v env in
    let (s3, v3) = pg bx1 s1 s2 env in
    (s3, v2)
