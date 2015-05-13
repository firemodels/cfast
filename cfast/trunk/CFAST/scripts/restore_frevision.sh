#!/bin/bash
dir=$1
file=$2

if [ ! -e $dir ] ; then
  exit
fi

fullfile=$dir/$file
fullfilebak=$dir/${file}bak
if [ -e $fullfilebak ] ; then
  cp $fullfilebak $fullfile
fi


