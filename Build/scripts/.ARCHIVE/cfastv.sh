#!/bin/bash
INPUT=$1
CFAST=$HOME/firemodels/cfast/Build/CFAST/intel_linux/cfast7_linux

$CFAST $INPUT -v
