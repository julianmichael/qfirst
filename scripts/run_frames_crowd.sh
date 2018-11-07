#!/bin/bash

BASE=`dirname $0`/..
pushd $BASE
{ echo ":load scripts/frames_crowd.scala" & cat <&0; } | mill -i qfirst.jvm.console
popd

