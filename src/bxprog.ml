open Syntax

let skip1 = Skip (fun k -> Unit)

let rearrS_d = RearrS(
    (fun m -> Con(m, m)), 
    (fun x -> match x with Con(m, n) when m = n -> m | _ -> assert false), 
    Replace
  )

let rearrV_d = RearrV(
    (fun m -> Con(m, m)),
    (fun x -> match x with Con(m, n) when m = n -> m | _ -> assert false), 
    Replace
  )

let prod_rs = Prod(Replace, skip1)

let phead = RearrS(
    (fun x -> match x with Con(s, ss) -> Con(s, ss) | _ -> assert false),
    (fun x -> match x with Con(s, ss) -> Con(s, ss) | _ -> assert false),
    RearrV(
      (fun v -> Con(v, Unit)), 
      (fun x -> match x with Con(v, Unit) -> v | _ -> assert false), 
      Prod(Replace, skip1)
    ) 
  ) 

let ptail = RearrS(
    (fun x -> match x with Con(s, ss) -> Con(s, ss) | _ -> assert false),
    (fun x -> match x with Con(s, ss) -> Con(s, ss) | _ -> assert false),
    RearrV(
      (fun v -> Con(Unit, v)), 
      (fun x -> match x with Con(Unit, v) -> v | _ -> assert false), 
      Prod(skip1, Replace)
    ) 
  ) 

let phead2 = RearrS(
    (fun x -> match x with Con(s, ss) -> Con(s, ss) | _ -> assert false),
    (fun x -> match x with Con(s, ss) -> Con(s, ss) | _ -> assert false),
    RearrV(
      (fun x -> match x with Con(v, Unit) -> Con(v, Unit) | _ -> assert false),
      (fun x -> match x with Con(v, Unit) -> Con(v, Unit) | _ -> assert false),
      Prod(Replace, skip1)
    )
  )

let replaceAll = Case(
    (fun s v -> match s with Con(_, Unit) -> true | _ -> false),
    (fun s -> match s with Con(_, Unit) -> true | _ -> false),
    RearrS(
      (fun x -> match x with Con(x, Unit) -> x | _ -> assert false),
      (fun x -> Con(x, Unit)), 
      Replace
    ),
    RearrV(
      (fun v -> Con(v, v)),
      (fun x -> match x with Con(v1, v2) when v1 = v2 -> v1 | _ -> assert false),
      Prod(Replace, Var("replaceAll"))
    )
  )

let replaceAlldef = Def("replaceAll", replaceAll, Var("replaceAll"))

let rec lassoc_comp bx n = match n with
  | 0 -> bx
  | n when n > 0 -> Compose(lassoc_comp bx (n - 1), bx)
  | _ -> assert false

let rec rassoc_comp bx n = match n with
  | 0 -> bx
  | n when n > 0 -> Compose(bx, rassoc_comp bx (n - 1))
  | _ -> assert false

let bsnoc = Case(
    (fun s v -> match v with Con(_, Con(_))-> false | Con(_, Unit) -> true | _ -> assert false),
    (fun s -> match s with Con(_, s) -> s = Unit | _ -> assert false),
    RearrV(
      (fun x -> match x with Con(v, Unit) -> Con(v, Unit) | _ -> assert false),
      (fun x -> match x with Con(v, Unit) -> Con(v, Unit) | _ -> assert false),
      Replace
    ),
    RearrS(
      (fun x -> match x with Con(x, Con(y, ys)) -> Con(y, Con(x, ys)) | _ -> assert false),
      (fun x -> match x with Con(y, Con(x, ys)) -> Con(x, Con(y, ys)) | _ -> assert false), 
      Prod(Replace, Var "bsnoc")
    )
  )

let bsnoc_def = Def("bsnoc", bsnoc, Var "bsnoc")

let rec bfoldr bx = match bx with
  | Var fname ->
    Case(
      (fun s v -> match s with Con(x,_) -> x = Unit | _ -> assert false),
      (fun s -> match s with Con(x, _) -> x = Unit | _ -> assert false),
      RearrV(
        (fun v -> (Con(Unit, v))), 
        (fun x -> match x with (Con(Unit, v)) -> v | _ -> assert false), 
        Prod(skip1, Replace)
      ), 
      RearrS(
        (fun x -> match x with Con(Con(x, xs), e) -> Con(x, Con(xs, e)) | _ -> assert false), 
        (fun x -> match x with Con(x, Con(xs, e)) -> Con(Con(x, xs), e) | _ -> assert false), 
        Compose(
          Prod(Replace, Var ("bfoldr_" ^ fname)), 
          Var fname
        )
      )
    )
  | _ -> assert false

let bfoldr_bsnoc = bfoldr (Var "bsnoc")

let bfoldr_bsnoc_def = Def("bfoldr_bsnoc", bfoldr_bsnoc,  Def("bsnoc", bsnoc, Var "bfoldr_bsnoc"))

let breverse = RearrS(
    (fun s -> Con(s, Unit)),
    (fun x -> match x with Con(s, Unit) -> s | _ -> assert false),
    bfoldr_bsnoc_def
  )

let rep2 = RearrV(
    (fun x -> match x with Con(v, vs) -> Con(v, vs) | _ -> assert false),
    (fun x -> match x with Con(v, vs) -> Con(v, vs) | _ -> assert false),
    Prod(Replace, Replace)
  )

let bfoldr_rep2 = bfoldr (Var "rep2")

let bfoldr_rep2_def = Def("bfoldr_rep2", bfoldr_rep2,  Def("rep2", rep2, Var "bfoldr_rep2"))

let bmapreplace = RearrS(
    (fun s -> Con(s, Unit)),
    (fun x -> match x with Con(s, Unit) -> s | _ -> assert false),
    bfoldr_rep2_def
  )
