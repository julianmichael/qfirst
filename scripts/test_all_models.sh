#!/bin/bash
BASE=`dirname $0`
echo "Testing models in $1/test..."
find $1/test -name config.json -exec $BASE/test_model.sh {} \;
