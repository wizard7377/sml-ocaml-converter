#!/bin/sh
format="%p "
fun_files=$(find ./examples/twelf/src -name "*.fun" -printf "$format")
sig_files=$(find ./examples/twelf/src -name "*.sig" -printf "$format")
sml_files=$(find ./examples/twelf/src -name "*.sml" -printf "$format")
simple_fun_files=$(find ./examples/simple/ -name "*.fun" -printf "$format")
simple_sml_files=$(find ./examples/simple/ -name "*.sml" -printf "$format")
simple_sig_files=$(find ./examples/simple/ -name "*.sig" -printf "$format")
all_files=$fun_files 
all_files+=$sig_files 
all_files+=$sml_files
all_simple_files=$simple_fun_files
all_simple_files+=$simple_sig_files
all_simple_files+=$simple_sml_files

echo "Running tests..."
dune exec shibboleth -- file $all_simple_files