#!/bin/bash

ocamlc definitions.ml
ocamlc shared.ml
ocamlc shared.cmo NFAtoDFA.ml
ocamlc shared.cmo minimizeDFA.ml
ocamlc shared.cmo minimizeDFA.cmo DifferenceDFA.ml
ocamlc shared.cmo minimizeDFA.cmo DifferenceDFA.cmo NFAtoDFA.cmo REXPtoDFA.ml