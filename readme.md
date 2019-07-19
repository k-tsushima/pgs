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
+--> pg.ml	
+--> test_pg.ml
|
+--> kpg.ml	
+--> test_kpg.ml
|
+--> cpg.ml	
+--> test_cpg.ml
|
+--> xpg.ml	
+--> test_xpg.ml
|
+--> main.ml (use when evalutating time)

```



### For testing in Ocaml top-level
1. use ```make```
2. in Ocaml top-level:
```
#load "syntax.cmo";;
#load "bxprog.cmo";;
#load "utils.cmo";;

#load "minbigul.cmo";;
#use "test_minbigul.ml";;

#load "pg.cmo";;
#use "test_pg.ml";;

#load "kpg.cmo";;
#use "test_kpg.ml";;

#load "cpg.cmo";;
#use "test_cpg.ml";;

#load "xpg.cmo";;
#use "test_xpg.ml";;
```