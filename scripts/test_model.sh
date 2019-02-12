#!/bin/bash
DIR=`dirname $1`
rm -rf $DIR/save
if allennlp train $1 --include-package qfirst -s $DIR/save &> /dev/null ; then
  echo "Success: $DIR"
else
  echo "FAILURE: $DIR/save/stderr.log"
fi
