#!/bin/bash

LD_PRELOAD=libgslcblas.so mill -i qfirst.frame.jvm.runMain qfirst.frame.FrameInductionApp "$@" 2>>extra.log
