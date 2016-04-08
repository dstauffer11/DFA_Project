# KATlite makefile
#
# targets are:
#
# all -- rebuild the project (default)
# clean -- remove all objects and executables

SOURCES = util.ml ast.ml Parser/katparser.mli Parser/katlexer.ml Parser/katparser.ml match.ml proof.ml state.ml command.ml repl.ml
OBJECTS = util.cmo ast.cmo Parser/katlexer.cmo Parser/katparser.cmo match.cmo proof.cmo state.cmo command.cmo repl.cmo

all: kat kattop

clean:
	rm -f kat kattop
	for X in . Parser; do \
      for Y in cmo cmi output log aux; do \
        rm -f $$X/*.$$Y; \
      done; \
    done

kat: $(SOURCES)
	/usr/local/bin/ocamlc -o kat -g -I Parser str.cma $(SOURCES)

kattop: $(OBJECTS)
	/usr/local/bin/ocamlmktop -o kattop -I Parser str.cma $(OBJECTS)

Parser/katparser.mli Parser/katparser.ml: Parser/katparser.mly
	/usr/local/bin/ocamlyacc -v Parser/katparser.mly

Parser/katlexer.ml: Parser/katlexer.mll Parser/katparser.ml
	/usr/local/bin/ocamllex Parser/katlexer.mll
