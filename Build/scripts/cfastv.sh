#!/bin/bash
INPUT=$1
CFAST=$HOME/firemodels/cfast/Build/CFAST/intel_linux_64/cfast7_linux_64

$CFAST $INPUT -v
