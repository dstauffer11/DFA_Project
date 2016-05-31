#!/bin/bash

# File to compile each file of the project with its dependencies

ocamlc util.ml
ocamlc util.cmo definitions.ml
ocamlc util.cmo definitions.cmo regexlexer.cmo regexparser.cmo nFAtoDFA.ml
ocamlc util.cmo definitions.cmo nFAtoDFA.cmo minimizeDFA.ml
ocamlc util.cmo definitions.cmo nFAtoDFA.cmo minimizeDFA.cmo differenceDFA.ml
ocamlc util.cmo definitions.cmo nFAtoDFA.cmo minimizeDFA.cmo differenceDfa.cmo rEXPtoDFA.ml
ocamlc util.cmo definitions.cmo nFAtoDFA.cmo minimizeDFA.cmo dFAtoREXP.ml


ocamlfind ocamlc -package oUnit -linkpkg -o unitTesting util.cmo definitions.cmo nFAtoDFA.cmo minimizeDFA.cmo differenceDFA.cmo rEXPtoDFA.cmo dFAtoREXP.cmo unitTesting.ml
./unitTesting