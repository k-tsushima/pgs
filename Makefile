SOURCES = \
	src/syntax.ml src/bxprog.ml src/utils.ml \
	src/minbigul.ml src/kpg.ml src/cpg.ml  src/xpg.ml src/xpg2.ml src/pg.ml src/pg2.ml \
	src/test_minbigul.ml src/test_kpg.ml src/test_cpg.ml src/test_xpg.ml src/test_xpg2.ml src/test_pg.ml src/test_pg2.ml\
	src/main.ml
RESULT = calc
OCAMLMAKEFILE = ./OCamlMakefile
include $(OCAMLMAKEFILE)
