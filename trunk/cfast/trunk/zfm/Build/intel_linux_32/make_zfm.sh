#!/bin/bash
source $IFORT_COMPILER/bin/iccvars.sh ia32
source $IFORT_COMPILER/bin/ifortvars.sh ia32
rm -f *.o *mod
make -f ../Makefile intel_linux_32
