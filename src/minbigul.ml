open Syntax
open Utils

(* let count_get = ref 0
let count_put = ref 0 *)

let rec get (bx:bigul) s env =
  (* count_get := !count_get + 1; *)
  match bx with
  | Def(name, bx1, bx2) ->
    get bx2 s ((name, bx1)::env)
  | Var(name) -> ( 
      try
        let bx = snd (List.find (fun (x, t) -> x = name) env) in
        get bx s env
      with Not_found -> 
        Printf.printf "%s is not found" name; 
        assert false
    )
  | Skip (h) ->
    h s
  | Replace ->
    s
  | Prod(bx1, bx2) ->
    let v1 = get bx1 (first s) env in
    let v2 = get bx2 (second s) env in
    Con(v1, v2)
  | RearrS(f1, f2, bx) ->
    let v = get bx ((fun m -> f1 m) s) env in
    v
  | RearrV(g1, g2, bx) ->
    let v = get bx s env in
    (fun m -> g2 m) v
  | Case(condsv, conds, bx1, bx2) ->
    if conds s then 
      let v = get bx1 s env in 
        if condsv s v then 
            v 
        else 
            assert false
    else
      let v = get bx2 s env in 
        if not (condsv s v) then 
            v 
        else
            assert false
  | Compose(bx1, bx2) ->
    let v1 = get bx1 s env in
    let v2 = get bx2 v1 env in
    v2

let rec put (bx:bigul) s v env =
  (* count_put := !count_put + 1; *)
  match bx with
  | Def(name, bx1, bx2) ->
    put bx2 s v ((name, bx1)::env)
  | Var(name) -> (
      try
        let bx = snd (List.find (fun (x, t) -> x = name) env) in
        put bx s v env
      with Not_found -> 
        Printf.printf "%s is not found" name; 
        assert false
    )
  | Skip (h) ->
    if h s = v then 
      s
    else
      assert false          
  | Replace -> 
    v
  | Prod(bx1, bx2) ->
    let s1 = put bx1 (first s) (first v) env in
    let s2 = put bx2 (second s) (second v) env in
    Con(s1, s2)
  | RearrS(f1, f2, bx) ->
    let s = put bx ((fun m -> f1 m) s) v env in
    (fun m -> f2 m) s
  | RearrV(g1, g2, bx) ->
    let s = put bx s ((fun m -> g1 m) v) env in
    s
  | Case(condsv, conds, bx1, bx2) ->
    if condsv s v then 
      let s' = put bx1 s v env in 
        if conds s' then 
            s'
        else 
            assert false
    else 
      let s' = put bx2 s v env in 
        if not (conds s') then 
            s' 
        else 
            assert false
  | Compose(bx1, bx2) ->
    let v1 = get bx1 s env in
    let s2 = put bx2 v1 v env in
    let s3 = put bx1 s s2 env in
    s3