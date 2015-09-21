#!/bin/sh

BASEDIR=$(dirname $0)

mpiexec -np 2 ccl -b -l "$BASEDIR/test.lisp"
