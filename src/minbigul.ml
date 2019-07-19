open Syntax

let count_g = ref 0
let count_p = ref 0
let count_c = ref 0

let rec get (bx:bigul) s env =
  count_g := !count_g + 1;
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
    let v1 = get bx1 ((fun x -> match x with (Con(s1, s2)) -> s1 | _ -> assert false) s) env in
    let v2 = get bx2 ((fun x -> match x with (Con(s1, s2)) -> s2 | _ -> assert false) s) env in
    Con(v1, v2)
  | RearrS(f1, f2, bx) ->
    let v = get bx ((fun m -> f1 m) s) env in
    v
  | RearrV(g1, g2, bx) ->
    let v = get bx s env in
    (fun m -> g2 m) v
  | Case(condsv, conds, bx1, bx2) ->
    if conds s then 
      get bx1 s env
    else
      get bx2 s env
  | Compose(bx1, bx2) ->
    let v1 = get bx1 s env in
    let v2 = get bx2 v1 env in
    v2

let rec put (bx:bigul) s v env =
  count_p := !count_p + 1;
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
    let s1 =
      put
        bx1
        ((fun x -> match x with (Con(s1, s2)) -> s1 | _ -> assert false) s)
        ((fun x -> match x with (Con(v1, v2)) -> v1 | _ -> assert false) v)
        env
    in
    let s2 =
      put
        bx2
        ((fun x -> match x with (Con(s1, s2)) -> s2 | _ -> assert false) s)
        ((fun x -> match x with (Con(v1, v2)) -> v2 | _ -> assert false) v)
        env
    in
    Con(s1, s2)
  | RearrS(f1, f2, bx) ->
    let s = put bx ((fun m -> f1 m) s) v env in
    (fun m -> f2 m) s
  | RearrV(g1, g2, bx) ->
    let s = put bx s ((fun m -> g1 m) v) env in
    s
  | Compose(bx1, bx2) ->
    count_c := !count_c + 1;
    let v1 = get bx1 s env in
    let s2 = put bx2 v1 v env in
    let s3 = put bx1 s s2 env in
    s3
  | Case(condsv, conds, bx1, bx2) ->
    if condsv s v then 
      put bx1 s v env
    else 
      put bx2 s v env