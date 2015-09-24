#!/bin/sh

BASEDIR=$(dirname $0)

if [ ! $1 ] || [ $1 = "--help" ]
then
    echo "Usage: $0 [all|clean|ecl|ccl|...]"
    exit
fi

if [ $1 = "clean" ]
then
    rm -f $BASEDIR/*-cl-mpi-image
    exit
fi

if [ $1 = "all" ]
then
    LISP_IMPLEMENTATIONS="sbcl ccl ecl"
else
    LISP_IMPLEMENTATIONS=$1
fi

for LISP in $LISP_IMPLEMENTATIONS; do
    IMAGE=$BASEDIR/$LISP-cl-mpi-image

    if [ ! -e $IMAGE ]
    then
    echo "/========================================"
    echo "Building $LISP image $IMAGE..."

        cl-launch --lisp $LISP \
                  -Q -s cl-mpi -s cl-mpi-testsuite -s mpi-benchmarks \
                  -o $IMAGE -d $IMAGE -L $BASEDIR/cl-mpi-test.lisp -E main
    echo "...$LISP image complete"
    echo "\========================================"
    echo
    fi

    echo "/========================================"
    echo "Testing $LISP..."

    mpiexec -npernode 4 $IMAGE
    echo "...$LISP testing complete"
    echo "\========================================"
    echo
done
