#!/bin/bash


# bash file to compile all the compilations with dependencies
# requires ocamlfindlib and ounit to compile that last line, a unit testing file
ocamlc definitions.ml
ocamlc definitions.cmo shared.ml
ocamlc definitions.cmo shared.cmo NFAtoDFA.ml
ocamlc definitions.cmo shared.cmo NFAtoDFA.cmo minimizeDFA.ml
ocamlc definitions.cmo shared.cmo NFAtoDFA.cmo minimizeDFA.cmo DifferenceDFA.ml
ocamlc definitions.cmo shared.cmo NFAtoDFA.cmo minimizeDFA.cmo differencedfa.cmo REXPtoDFA.ml


ocamlfind ocamlc -package oUnit -linkpkg -o unitTesting definitions.cmo shared.cmo NFAtoDFA.cmo minimizeDFA.cmo DifferenceDFA.cmo REXPtoDFA.cmo unitTesting.ml
./unitTesting