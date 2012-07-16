SOURCES = loader.ml
LIBS = unix
PACKS = extlib
RESULT = loader
REALLY_QUIET = yes

OCAMLC = ocamlc -unsafe 
OCAMLOPT = ocamlopt -unsafe -inline 9999

#TARGET:debug-code
TARGET:native-code

include OCamlMakefile

.SILENT:

run:$(RESULT)
	OCAMLRUNPARAM=b ./loader

