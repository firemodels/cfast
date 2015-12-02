#!/bin/bash
FROMDIR=$1

GDRIVE=~/bin/gdrive
CURDIR=`pwd`
# directory containing guides on google drive : CFAST Newest Manuals
PARENT_ID=0B-W-dkXwdHWNTWtIWnVtUm95V3M

UPLOAD ()
{
  FILE=$1
  FILEnew=${FILE}_new.pdf
  cp $FILE.pdf $FILEnew
  $GDRIVE list  | grep $FILEnew | awk '{ system("~/bin/gdrive delete -i " $1)} '
  $GDRIVE upload -p $PARENT_ID -f $FILEnew
}

if [ -e $GDRIVE ] ; then
  cd $FROMDIR
  UPLOAD CFAST_Tech_Ref
  UPLOAD CFAST_Validation_Guide
  cd $CURDIR
fi
