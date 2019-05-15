
let test0 =
  let ((ks, s), (kv, v)) = kpg (Skip(fun m -> m)) id id (Int (3)) (Int (3)) in
  let s = ks s in
  let v = kv v in
  (s, v) = (Int (3), Int (3))

  
let test1 =
  let ((ks, s), (kv, v)) =
    kpg Replace (fun m -> m) (fun m -> m) (Int(3)) (Int(5)) in
  let s = ks s in
  let v = kv v in
  (s, v) = (Int (5), Int (3))
             
let test2 =
  let ((ks, s), (kv, v)) = kpg (Prod(Replace, Replace)) (fun m -> m) (fun m -> m) (Con(Int(2), Int(3))) (Con(Int(5), Int(4))) in
  let s = ks s in
  let v = kv v in
  (s, v) = 
    ((Con(Int(5), Int(4))), (Con(Int(2), Int(3))))
              
let test3 =
  let ((ks, s), (kv, v)) = kpg (RearrS((fun m -> Con(m, m)), (fun (Con(m, n)) -> if m = n then m else assert false), Replace)) id id (Int(3)) (Con(Int(4), Int(4))) in
  let s = ks s in
  let v = kv v in 
  (s, v) = (Int(4), (Con(Int 3, Int 3)))

let test4 =
  let ((ks, s), (kv, v)) = kpg (RearrV((fun m -> Con(m, m)), (fun (Con(m, n)) -> if m = n then m else assert false), Replace)) id id (Con(Int(4), Int(4))) (Int(3))  in
  let s = ks s in
  let v = kv v in
  (s, v) = ((Con(Int 3, Int 3)), (Int(4)))
             
let test5 =
  let ((ks, s), (kv, v)) = kpg (Compose(Replace, Replace)) id id (Con(Int(4), Int(4))) (Int(4)) in
  let s = ks s in
  let v = kv v in
  (s, v) = ((Int(4)), (Con(Int(4), Int(4))))

let test6 =
  let ((ks, s), (kv, v)) = kpg (Compose(Replace, (Skip (fun m -> Int(3))))) id id (Con(Int(4), Int(4))) (Int(3)) in
  let s = ks s in
  let v = kv v in
  (s, v) = (Con (Int 4, Int 4), Int 3)

let test_skip1 =
  let ((ks, s), (kv, v)) = kpg skip1 id id (Con(Int(3), Int(10))) Unit in
  let s = ks s in
  let v = kv v in
  (s, v)

let test_rep2 = 
  let ((ks, s), (kv, v)) = kpg rep2 id id (Con(Int(3), Int(10))) (Con(Int 1, Unit)) in
  let s = ks s in
  let v = kv v in
  (s, v)

let test_body_phead = 
  let ((ks, s), (kv, v)) = kpg body_phead id id (Con(Int(3), Int(10))) (Con(Int 1, Unit)) in
  let s = ks s in
  let v = kv v in
  (s, v) = (Con (Int 1, Int 10), Con (Int 3, Unit))

let test_case =
  let prog = (Case((fun s v -> match s with Con(Con(_, _), _) -> false | _ -> true), Replace, Replace)) in
  let ((ks, s), (kv, v)) = kpg prog id id (Con(Int(3), Int(10))) (Con(Int 1, Unit)) in
  let s = ks s in
  let v = kv v in
  (s, v)

(* (* cause Errror *)
let test_phead2 =
  let ((ks, s), (kv, v)) = kpg phead2 id id (Con(Int(3), Int(10))) (Int(1)) in
  let s = ks s in
  let v = kv v in
  (s, v) = (Con (Int 1, Int 10), Int 3)
 *)

let test_phead =
  let ((ks, s), (kv, v)) = kpg phead id id (Con(Int(3), Int(10))) (Int(1)) in
  let s = ks s in
  let v = kv v in
  (s, v) = (Con (Int 1, Int 10), Int 3)


             
  
let test_phead1 =
  let ((ks, s), (kv, v)) = kpg (make_long_bx 0) id id (Con(Int 1, (Con(Int 2, Int 3)))) (Int 200) in
  let s = ks s in
  let v = kv v in
  (s, v)

let test_phead21 =
  let ((ks, s), (kv, v)) = kpg phead id id (Con((Con(Int 1, Int 2)), (Con(Int 3, (Con(Int 4, Int 5)))))) (Int 100) in
  let s = ks s in
  let v = kv v in
  (s, v)

let test_phead22 =
  let ((ks, s), (kv, v)) = kpg phead id id (Con((Int 100, Con (Int 3, Con (Int 4, Int 5))))) (Con (Int 1, Int 2)) in
  let s = ks s in
  let v = kv v in
  (s, v)

 let ((ks1, s1), (kv1, v1)) = kpg phead id id
                            (Con((Con(Int 1, Con(Int 2, Int 9))), (Con(Int 3, (Con(Int 4, Int 5)))))) Dummy
 let ((ks, s), (kv, v)) = kpg phead id id (Con (Int 1, Con (Int 2, Int 9))) (Int 100) 
  
let test_phead2 =
  let ((ks, s), (kv, v)) = kpg (Compose(phead, phead)) id id
                             (Con((Con(Int 1, Con(Int 2, Int 9))), (Con(Int 3, (Con(Int 4, Int 5)))))) (Int 100)  in
  let s = ks s in
  let v = kv v in
  (s, v) = (Con((Con(Int 100, Con(Int 2, Int 9))), (Con(Int 3, (Con(Int 4, Int 5))))), Int 1)

let test_phead3 =
  let ((ks, s), (kv, v)) = kpg (Compose(phead, (Compose(phead, phead)))) id id
                             (Con((Con(Con(Int 3, Int 5), Con(Int 2, Int 9))), (Con(Int 3, (Con(Int 4, Int 5)))))) (Int 100)  in
  let s = ks s in
  let v = kv v in
  (s, v) = (Con((Con(Int 100, Con(Int 2, Int 9))), (Con(Int 3, (Con(Int 4, Int 5))))), Int 1)


let test_pheadn n =
  let ((ks, s), (kv, v)) = kpg (make_long_bx n) id id
                             (make_lst (n + 1)) (Int 100) [] in
  let s = ks s in
  let v = kv v in
  if v = Int 1 then true else false
  
  

let test_phead2 =
  let ((ks, s), (kv, v)) = kpg (Compose(phead, phead)) id id
                             (Con((Con(Int 1, Int 2)), (Con(Int 3, (Con(Int 4, Int 5)))))) (Int 100)  in
  let s = ks s in
  let v = kv v in
  (s, v)

let test_replaceAll =
  let ((ks, s), (kv, v)) = kpg replaceAll id id (Con(Int 1, (Con(Int 1, Unit)))) (Int 200) in
  let s = ks s in
  let v = kv v in
  (s, v) = (Con (Int 200, Con (Int 200, Unit)), Int 1)

let test_replaceAll2 =
  let ((ks, s), (kv, v)) = kpg (Compose(replaceAll, replaceAll)) id id (Con((Con(Int 1, Unit)), (Con((Con(Int 1, Unit)), Unit)))) (Int 200) in
  let s = ks s in
  let v = kv v in
  (s, v) = (Con (Con (Int 200, Unit), Con (Con (Int 200, Unit), Unit)), Int 1)
