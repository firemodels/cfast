#!/bin/csh -f
set host=$1
set dir=$2
set command=$3
ssh -q $host bash -lc $dir/$command
