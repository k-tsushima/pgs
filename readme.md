# An efficient composition of bidirectional programs by memoization and lazy update

### Anonymous authors

---

## Environments
- OS supporting OCaml
- OCaml 4.07.1

---

## Source code structure
```
src 
+--> syntax.ml
+--> bxprog.ml 
+--> utils.ml (contain some functions about dummy, data)
|
+--> minibigul.ml	
+--> test_minibigul.ml
|
+--> minibigul_m.ml	
+--> test_minibigul_m.ml
|
+--> pg.ml	
+--> test_pg.ml
|
+--> cpg.ml	
+--> test_cpg.ml
|
+--> kpg.ml	
+--> test_kpg.ml
|
+--> xpg.ml
+--> test_xpg.ml
|
+--> main.ml (use for evaluation)
```

---

## Testing in OCaml top-level

1. go to ```src```
2. use ```OCaml top-level```
```ocaml
#load "syntax.cmo";;
#load "bxprog.cmo";;
#load "utils.cmo";;

#load "minbigul.cmo";;
#use "test_minbigul.ml";;

#load "minbigul_m.cmo";;
#use "test_minbigul_m.ml";;

#load "pg.cmo";;
#use "test_pg.ml";;

#load "cpg.cmo";;
#use "test_cpg.ml";;

#load "kpg.cmo";;
#use "test_kpg.ml";;

#load "xpg.cmo";;
#use "test_xpg.ml";;
```

---

## Evaluating time & byte allocation

### Executing
1. ```make```

2. ```./calc method fname ncomp```
    - method:
        ```
        [put|put_m|pg|cpg|kpg|xpg]
        ```

    - fname:
        ```
        [l|r]assoc_comp_[replace|phead|phead_2|ptail|bsnoc](_ldata)?

        breverse(_ldata)?, bmapreplace(_ldata)?
        ```

    - ncomp: number of compositions

### Examples
    ./calc put lassoc_comp_phead 100
    ./calc pg rassoc_comp_ptail_ldata 100
    ./calc put_m breverse 100
    ./calc xpg breverse_ldata 100

### Results (calculated by Gc Module)

    Elapsed Time: x.xxxxxx [s]
    Allocated Bytes: xxxxxx [bytes]

---

## Writing tests

1. **put**
    ```ocaml
    let s = $s_value$ in
    let v = $v_value$ in
    let bx = $bx_value$ in
    let s' = put bx s v [] in 
        s'
    ```
2. **get**
    ```ocaml
    let s = $s_value$ in
    let bx = $bx_value$ in
    let v = get bx s v [] in 
        v
    ```
3. **put_m**
    ```ocaml
    let s = $s_value$ in
    let v = $v_value$ in
    let bx = $bx_value$ in
    let s' = put_m bx s v [] in 
        s'
    ```
4. **get_m**
    ```ocaml
    let s = $s_value$ in
    let bx = $bx_value$ in
    let v = get_m bx s v [] in 
        v
    ```
5. **pg**
    ```ocaml
    let s = $s_value$ in
    let v = $v_value$ in
    let bx = $bx_value$ in
    let (s, v) = pg bx s v [] in 
        (s, v)
    ```
6. **cpg**
    ```ocaml
    let s = $s_value$ in
    let v = $v_value$ in
    let bx = $bx_value$ in
    let (ks, kv, s, v) = cpg bx (fun _ -> s) id s v [] in 
        (s, v)
    ```
7. **kpg**
    ```ocaml
    let s = $s_value$ in
    let v = $v_value$ in
    let bx = $bx_value$ in
    let (ks, kv, ks', kv', s', v') = kpg bx (fun _ -> s) id id id s v [] in 
        (ks' s', kv' v')
    ```
8. **xpg**
    ```ocaml
    let s = $s_value$ in
    let v = $v_value$ in
    let bx = $bx_value$ in
    let (s, v) = xpg bx s v [] in 
        (s, v)
    ```