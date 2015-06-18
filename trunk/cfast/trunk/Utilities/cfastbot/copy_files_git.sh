#!/bin/bash
CURDIR=`pwd`
repo=cfastgitclean

cd ~/$repo
git pull
cd $CURDIR
cp ~/$repo/Utilities/cfastbot/*.sh .
