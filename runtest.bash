#!/usr/bin/bash
# first parameter is the directory to test
# second parameter is the test script to run (without .erl)
cp $2.erl $1
pushd $1
rm -f *.beam
erlc *.erl
erl -noshell -eval "$2:test(),init:stop()"
popd
# useful if you want to view the code too
# emacs $1/mergesort.erl

