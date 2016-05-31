# DFA makefile
#
# targets are:
#
# all -- rebuild the project (default)
# clean -- remove all objects and executables

# uncomment the following 2 lines for Eclipse/Mac OS X
export SHELL = /bin/bash
export PATH = /usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin

SOURCES = util.ml definitions.ml NFAtoDFA.ml minimizeDFA.ml differenceDFA.ml REXPtoDFA.ml unitTesting.ml
OBJECTS = util.cmo definitions.cmo NFAtoDFA.cmo minimizeDFA.cmo differenceDFA.cmo REXPtoDFA.cmo unitTesting.cmo \
          util.cmi definitions.cmi NFAtoDFA.cmi minimizeDFA.cmi differenceDFA.cmi REXPtoDFA.cmi unitTesting.cmi

all: dfa

clean:
	rm -f dfa
	rm -f $(OBJECTS)

dfa: $(SOURCES)
	ocamlfind ocamlc -o dfa -g -I str.cma -package oUnit -linkpkg $(SOURCES)
