SOURCES = \
	src/syntax.ml src/bxprog.ml src/utils.ml \
	src/minbigul.ml src/pg.ml src/cpg.ml src/kpg.ml src/xpg.ml \
	src/test_minbigul.ml src/test_pg.ml src/test_cpg.ml src/test_kpg.ml src/test_xpg.ml \
	src/main.ml
RESULT = calc
OCAMLMAKEFILE = ./OCamlMakefile
include $(OCAMLMAKEFILE)
