#!/bin/bash

# File to compile each file of the project with its dependencies

ocamlc definitions.ml
ocamlc definitions.cmo shared.ml
ocamlc definitions.cmo shared.cmo NFAtoDFA.ml
ocamlc definitions.cmo shared.cmo NFAtoDFA.cmo minimizeDFA.ml
ocamlc definitions.cmo shared.cmo NFAtoDFA.cmo minimizeDFA.cmo DifferenceDFA.ml
ocamlc definitions.cmo shared.cmo NFAtoDFA.cmo minimizeDFA.cmo differenceDfa.cmo REXPtoDFA.ml
ocamlc definitions.cmo shared.cmo NFAtoDFA.cmo minimizeDFA.cmo DFAtoREXP.ml


ocamlfind ocamlc -package oUnit -linkpkg -o unitTesting definitions.cmo shared.cmo NFAtoDFA.cmo minimizeDFA.cmo DifferenceDFA.cmo REXPtoDFA.cmo DFAtoREXP.cmo unitTesting.ml
./unitTesting