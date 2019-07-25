open Syntax
open Utils
open Xpg

let rec pg2 (bx:bigul) s v env =
  match bx with
  | Def(name, bx1, bx2) ->
    pg2 bx2 s v ((name, bx1)::env)
  | Var(name) -> (
      try
        let bx = snd (List.find (fun (x, t) -> x = name) env) in
        pg2 bx s v env
      with Not_found -> 
        Printf.printf "%s is not found" name; 
        assert false
    )
  | Skip (h) ->
    (s, if h s = v then v else assert false)
  | Replace ->
    (v, s)
  | Prod(bx1, bx2) ->
    let (s1, v1) =
      pg2
        bx1
        ((fun x -> match x with (Con(s1, s2)) -> s1 | _ -> assert false) s)
        ((fun x -> match x with (Con(v1, v2)) -> v1 | _ -> assert false) v)
        env
    in
    let (s2, v2) = 
      pg2
        bx2
        ((fun x -> match x with (Con(s1, s2)) -> s2 | _ -> assert false) s)
        ((fun x -> match x with (Con(v1, v2)) -> v2 | _ -> assert false) v)
        env
    in
    (Con(s1, s2), Con(v1, v2))
  | RearrS(f1, f2, bx) ->
    let (s, v) = pg2 bx ((fun m -> f1 m) s) v env in
    ((fun m -> f2 m) s, v)
  | RearrV(g1, g2, bx) ->
    let (s, v) = pg2 bx s ((fun m -> g1 m) v) env in
    (s, (fun m -> g2 m) v)
  | Case(condsv, conds, bx1, bx2) ->
    if condsv s v then
      pg2 bx1 s v env
    else
      pg2 bx2 s v env
  | Compose(bx1, bx2) ->
    let (ks, kv, ks', kv', s', v') = xpg bx1 (fun _ -> s) id id id s v env in 
    let (s'', v'') = pg2 bx2 (kv' v') v env in
        (ks s'', v'')