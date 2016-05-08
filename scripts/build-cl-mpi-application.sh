#!/bin/sh

usage()
{
    echo "Usage: $0 IN OUT"
    echo ""
    echo -n "Build a standalone executable named OUT that will execute"
    echo "the function CL-USER::MAIN when launched."
}

if [ ! $1 ] || [ ! $2 ]
then
    usage
    exit
fi

if [ ! `which cl-launch` ]
then
    echo "This script requires cl-launch (http://cliki.net/CL-Launch)"
    exit
fi

IN=$1
OUT=$2

cl-launch \
    -Q -s cl-mpi -s cl-mpi-test-suite -o $OUT -d $OUT \
    -L $IN -E main
