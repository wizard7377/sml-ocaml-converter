#!/bin/sh
format="%p "
fun_files=$(find ./examples/src -name "*.fun" -printf "$format")
sig_files=$(find ./examples/src -name "*.sig" -printf "$format")
sml_files=$(find ./examples/src -name "*.sml" -printf "$format")
all_files=$fun_files 
all_files+=$sig_files 
all_files+=$sml_files

echo "Running tests..."
dune exec sml-ocaml-converter -- file $fun_files