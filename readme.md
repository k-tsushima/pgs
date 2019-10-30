# pgs

### Source code structure
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
+--> cpg2.ml (combine pg & cpg)	
+--> test_cpg2.ml
|
+--> kpg.ml	
+--> test_kpg.ml
|
+--> kpg2.ml (combine pg & kpg)	
+--> test_kpg2.ml
|
+--> main.ml (use for evaluation)
```



### For testing in OCaml top-level
1. use ```make```
2. in OCaml top-level:
```
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

#load "cpg2.cmo";;
#use "test_cpg2.ml";;

#load "kpg.cmo";;
#use "test_kpg.ml";;

#load "kpg2.cmo";;
#use "test_kpg2.ml";;
```