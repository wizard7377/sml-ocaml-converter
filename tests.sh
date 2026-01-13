#!/bin/sh

TEST_OPTS="$@"
rm -rf examples/output 
mkdir -p examples/output
if [[ -z $TEST_OPTS ]]; then
    TEST_OPTS="--verb=1"
    eval "dune exec shibboleth  -- group "$TEST_OPTS" --force --input examples/twelf --output examples/output"
else 
    eval "dune exec shibboleth  -- group "$TEST_OPTS" --force --input examples/twelf --output examples/output"
fi 