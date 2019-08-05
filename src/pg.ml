open Syntax
open Utils

let count = ref 0

let rec pg (bx:bigul) s v env =
  count := !count + 1;
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
    let (s1, v1) =
      pg
        bx1
        ((fun x -> match x with (Con(s1, s2)) -> s1 | _ -> assert false) s)
        ((fun x -> match x with (Con(v1, v2)) -> v1 | _ -> assert false) v)
        env
    in
    let (s2, v2) = 
      pg
        bx2
        ((fun x -> match x with (Con(s1, s2)) -> s2 | _ -> assert false) s)
        ((fun x -> match x with (Con(v1, v2)) -> v2 | _ -> assert false) v)
        env
    in
    (Con(s1, s2), Con(v1, v2))
  | RearrS(f1, f2, bx) ->
    let (s, v) = pg bx ((fun m -> f1 m) s) v env in
    ((fun m -> f2 m) s, v)
  | RearrV(g1, g2, bx) ->
    let (s, v) = pg bx s ((fun m -> g1 m) v) env in
    (s, (fun m -> g2 m) v)
  | Case(condsv, conds, bx1, bx2) ->
    if condsv s v then
      pg bx1 s v env
    else
      pg bx2 s v env
  | Compose(bx1, bx2) ->
    (* let (s1, v1) = pg bx1 s (construct_dummy v) env in *)
    let (s1, v1) = pg bx1 s v env in
    let (s2, v2) = pg bx2 v1 v env in
    let (s3, v3) = pg bx1 s1 s2 env in
    (s3, v2)
