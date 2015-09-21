#!/bin/sh

BASEDIR=$(dirname $0)

# compiling cl-mpi in parallel leads to errors, so I do a serial compilation
# run first.
sbcl --noinform --non-interactive --eval "(asdf:load-system :mpi-benchmarks)"

mpiexec -np 2 sbcl --non-interactive --load "$BASEDIR/benchmark.lisp"
