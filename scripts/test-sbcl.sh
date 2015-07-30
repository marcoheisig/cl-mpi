#!/bin/sh

# compiling cl-mpi in parallel leads to errors, so I do a serial compilation
# run first.
sbcl --noinform --non-interactive --eval "(asdf:load-system :cl-mpi-testsuite)"

mpiexec -np 2 sbcl --noinform --non-interactive --load "test.lisp"
