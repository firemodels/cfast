#!/bin/bash
source $IFORT_COMPILER/bin/iccvars.sh ia32
source $IFORT_COMPILER/bin/ifortvars.sh ia32
make -f ../Makefile intel_linux_32
