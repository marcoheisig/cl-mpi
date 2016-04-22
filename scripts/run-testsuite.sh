#!/bin/sh

BASEDIR=$(dirname $0)

usage()
{
    echo "Usage: $0 [all|clean|ecl|ccl|...]"
}

clean()
{
    rm -f $BASEDIR/*.image
}

build_image()
{
    LISP=$1
    IMAGE=$BASEDIR/$LISP.image
    LISP_VERSION=$($1 --version) || "unknown version"

    echo "========================================="
    echo "Building $LISP ($LISP_VERSION) image ..."

    if cl-launch --lisp $LISP \
                 -Q -s cl-mpi-test-suite \
                 -o $IMAGE -d $IMAGE -L \
                 $BASEDIR/run-testsuite.lisp -E main; then
        echo "...$LISP image complete"
        echo "========================================="
        echo
        return 0
    else
        echo "...$LISP image creation failed"
        echo "========================================="
        echo
        return 1
    fi
}

test_image()
{
    LISP=$1
    IMAGE=$BASEDIR/$LISP.image

    echo "========================================="
    echo "Testing $LISP..."

    mpiexec -n 4 $IMAGE
    echo "...$LISP testing complete"
    echo "========================================="
    echo
}



if [ ! $1 ] || [ $1 = "--help" ]
then
    usage
    exit
fi

if [ ! `which cl-launch` ]
then
    echo "This script requires cl-launch (http://cliki.net/CL-Launch)"
    exit
fi

if [ $1 = "clean" ]
then
    clean
    exit
fi

if [ $1 = "all" ]
then
    LISP_IMPLEMENTATIONS="sbcl ccl ecl"
else
    LISP_IMPLEMENTATIONS=$1
fi

for LISP in $LISP_IMPLEMENTATIONS; do
    if build_image $LISP; then
        test_image $LISP
    fi
done

clean

