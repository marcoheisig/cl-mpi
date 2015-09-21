#!/bin/sh

BASEDIR=$(dirname $0)

for lisp in ccl sbcl # ecl
do
    echo "Testing $lisp..."
    test `which $lisp` && $BASEDIR/test-$lisp.sh
done
