open Syntax
open Utils

let (table_g : (data * bigul, data) Hashtbl.t) = Hashtbl.create 0

let count_get_m_h = ref 0

let rec get_m (bx:bigul) s env =
  match bx with
  | Def(name, bx1, bx2) ->
    get_m bx2 s ((name, bx1)::env)
  | Var(name) -> (
      try
        let bx = snd (List.find (fun (x, t) -> x = name) env) in
        get_m bx s env
      with Not_found -> 
        Printf.printf "%s is not found" name; 
        assert false
    )
  | Skip (h) ->
    h s
  | Replace ->
    s
  | Prod(bx1, bx2) ->
    let v1 = get_m bx1 (first s) env in
    let v2 = get_m bx2 (second s) env in
    Con(v1, v2)
  | RearrS(f1, f2, bx) ->
    let v = get_m bx ((fun m -> f1 m) s) env in
    v
  | RearrV(g1, g2, bx) ->
    let v = get_m bx s env in
    (fun m -> g2 m) v
  | Case(condsv, conds, bx1, bx2) ->
    if conds s then 
      let v = get_m bx1 s env in 
      if condsv s v then 
        v 
      else
        assert false
    else
      let v = get_m bx2 s env in 
      if not (condsv s v) then
        v
      else
        assert false
  | Compose(bx1, bx2) ->
    let v1 = get_m_h bx1 s env in
    let v2 = get_m_h bx2 v1 env in
        try (Hashtbl.find table_g (v2, Compose(bx1, bx2))) 
        with Not_found ->
            Hashtbl.add table_g (s, bx) v2
        ;
        v2
and
  get_m_h bx s env = 
    count_get_m_h := !count_get_m_h + 1;
    try (Hashtbl.find table_g (s, bx)) 
    with Not_found ->
        let v = get_m bx s env in  
            (* count_get_m_h := !count_get_m_h + 1; *)
            Hashtbl.add table_g (s, bx) v;
            v

let rec put_m (bx:bigul) s v env =
  match bx with
  | Def(name, bx1, bx2) ->
    put_m bx2 s v ((name, bx1)::env)
  | Var(name) -> (
      try
        let bx = snd (List.find (fun (x, t) -> x = name) env) in
        put_m bx s v env
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
    let s1 = put_m bx1 (first s) (first v) env in
    let s2 = put_m bx2 (second s) (second v) env in
    Con(s1, s2)
  | RearrS(f1, f2, bx) ->
    let s = put_m bx ((fun m -> f1 m) s) v env in
    (fun m -> f2 m) s
  | RearrV(g1, g2, bx) ->
    let s = put_m bx s ((fun m -> g1 m) v) env in
    s
  | Case(condsv, conds, bx1, bx2) ->
    if condsv s v then 
      let s' = put_m bx1 s v env in 
      if conds s' then 
        s' 
      else
        assert false
    else 
      let s' = put_m bx2 s v env in 
      if not (conds s') then
        s' 
      else
        assert false
  | Compose(bx1, bx2) ->
    let v1 = get_m_h bx1 s env in
    let s2 = put_m bx2 v1 v env in
    let s3 = put_m bx1 s s2 env in
    s3