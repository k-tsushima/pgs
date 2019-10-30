open Syntax
open Utils

(*
	s = s0, v = v0
  xpg bx ks kv ks' kv' s' v' env = (ks, kv, ks', kv',s', v')
  where
    ks, kv ~ cpg 
    ks', kv', s', v' ~ kpg
*)

let rec xpg (bx:bigul) (ks:data->data) (kv:data->data) (ks':data->data) (kv':data->data) s' v' env = match bx with
  | Def(name, bx1, bx2) ->
    	xpg bx2 ks kv ks' kv' s' v' ((name, bx1) :: env)
  | Var(name) ->
     (try
	let bx = snd (List.find (fun (x, t) -> x = name) env) in
	xpg bx ks kv ks' kv' s' v' env
      with Not_found -> 
	Printf.printf "%s is not found" name; 
	assert false
     )
  | Skip(h) ->
    let r = ks' s' in
    let t = kv' v' in
    if h r = t then
      (ks, kv, id, id, r, t)
    else
      assert false
  | Replace ->
    (kv, ks, kv', ks', v', s')
  | Prod(bx1, bx2) ->
    let (ks1, kv1, ks1', kv1', s1', v1') = 
      xpg
        bx1
        (fun m -> (fun x -> match x with Con(s1, s2) -> s1 | _ -> assert false) (ks m)) 
        (fun m -> (fun x -> match x with Con(v1, v2) -> v1 | Dummy -> Dummy | _ -> assert false) (kv m))
        (fun m -> (fun x -> match x with Con(s1, s2) -> s1 | _ -> assert false) (ks' m))
        (fun m -> (fun x -> match x with Con(v1, v2) -> v1 | Dummy -> Dummy | _ -> assert false) (kv' m))
        s'
        v'
        env
    in
    let (ks2, kv2, ks2', kv2', s2', v2') = 
      xpg
        bx2
        (fun m -> (fun x -> match x with Con(s1, s2) -> s2 | _ -> assert false) (ks m)) 
        (fun m -> (fun x -> match x with Con(v1, v2) -> v2 | Dummy -> Dummy | _ -> assert false) (kv m))
        (fun m -> (fun x -> match x with Con(s1, s2) -> s2 | _ -> assert false) (ks' m))
        (fun m -> (fun x -> match x with Con(v1, v2) -> v2 | Dummy -> Dummy | _ -> assert false) (kv' m))
        s'
        v'
        env
    in
    (
      (fun x -> Con(ks1 x, ks2 x)),
      (fun x -> Con(kv1 x, kv2 x)),
      (fun x -> match x with Con(s1', s2') -> Con(ks1' s1', ks2' s2') | _ -> assert false),
      (fun x -> match x with Con(v1', v2') -> Con(kv1' v1', kv2' v2') | _ -> assert false),
      (Con(s1', s2')),
      (Con(v1', v2'))
    )
  | RearrS(f1, f2, bx) ->
    let (ks, kv, ks', kv', s', v') = 
      xpg
        bx
        (fun m -> f1 (ks m))
        kv
        (fun m -> f1 (ks' m)) 
        kv'
        s'
        v' 
        env
    in
    (
      (fun m -> f2 (ks m)), 
      kv, 
      (fun m -> f2 (ks' m)), 
      kv', 
      s', 
      v'
    )
  | RearrV(g1, g2, bx) ->
    let (ks, kv, ks', kv', s', v') = 
      xpg 
        bx 
        ks 
        (fun m -> g1 (kv m)) 
        ks' 
        (fun m -> g1 (kv' m)) 
        s'
        v'
        env 
    in
    (
      ks, 
      (fun m -> g2 (kv m)), 
      ks', 
      (fun m -> g2 (kv' m)), 
      s', 
      v'
    )
  | Case(condsv, conds, bx1, bx2) ->
    let (ks', s') = (id, ks' s') in
    let (kv', v') = (id, kv' v') in
			if (condsv s' v') then
				xpg bx1 ks kv ks' kv' s' v' env
			else
				xpg bx2 ks kv ks' kv' s' v' env
  | Compose(bx1, bx2) ->
    let (ks1, kv1, ks1', kv1', s1', v1') =
      xpg
        bx1
        ks
        id
        ks'
        id
        s'
        v' (* (construct_dummy v') *)
        env
    in
    let (ks2, kv2, ks2', kv2', s2', v2') =
      xpg
        bx2
        kv1
        kv
        kv1'
        kv'
        v1'
        v'
        env
    in
    (
      (fun x -> ks1 (ks2 x)),
      kv2,
      (fun m -> ks1 (ks2' m)),
      kv2',
      s2',
      v2'
    )
		
