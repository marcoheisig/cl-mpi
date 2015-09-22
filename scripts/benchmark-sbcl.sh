#!/bin/sh

BASEDIR=$(dirname $0)

$BASEDIR/internals/do benchmark sbcl
