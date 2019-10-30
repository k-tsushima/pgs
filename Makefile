SOURCES = \
	src/syntax.ml src/bxprog.ml src/utils.ml \
	src/minbigul.ml src/minbigul_m.ml src/pg.ml src/cpg.ml src/cpg2.ml src/kpg.ml src/kpg2.ml \
	src/test_minbigul.ml src/test_minbigul_m.ml src/test_pg.ml src/test_cpg.ml src/test_cpg2.ml src/test_kpg.ml src/test_kpg2.ml \
	src/main.ml
RESULT = calc
OCAMLMAKEFILE = ./OCamlMakefile
include $(OCAMLMAKEFILE)
