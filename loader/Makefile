SOURCES = loader.ml
LIBS = unix
PACKS = extlib
RESULT = loader
REALLY_QUIET = yes

TARGET:debug-code

include OCamlMakefile

.SILENT:

run:$(RESULT)
	OCAMLRUNPARAM=b ./loader

