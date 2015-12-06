#!/bin/bash
FROMDIR=$1
MANDIR=$2

GDRIVE=~/bin/gdrive
CURDIR=`pwd`
# directory containing guides on google drive : CFAST Newest Manuals
MANUAL_PARENT_ID=0B-W-dkXwdHWNTWtIWnVtUm95V3M
FIGURES_PARENT_ID=0B-W-dkXwdHWNNEl6S2hUR1Y0LXM

UPLOADGUIDE ()
{
  FILE=$1
  FILEnew=CFAST_${FILE}_new.pdf
  cp $FILE.pdf $FILEnew
  $GDRIVE list  | grep $FILEnew | awk '{ system("~/bin/gdrive delete -i " $1)} '
  $GDRIVE upload -p $MANUAL_PARENT_ID -f $FILEnew
}
UPLOADFIGURES ()
{
  DIRECTORY=$1
  FILE=$2
  cd $MANDIR/$DIRECTORY/SCRIPT_FIGURES
  tarfile=${FILE}_figures.tar
  rm -f ../$tarfile
  rm -f ../$tarfile.gz
  tar cvf ../$tarfile . &> /dev/null
  cd ..
  gzip $tarfile
  $GDRIVE list  | grep $tarfile.gz | awk '{ system("~/bin/gdrive delete -i " $1)} '
  $GDRIVE upload -p $FIGURES_PARENT_ID -f $tarfile.gz
}


if [ -e $GDRIVE ] ; then
  cd $FROMDIR
  UPLOADGUIDE Tech_Ref
  UPLOADGUIDE Validation_Guide
  UPLOADFIGURES Validation_Guide CFAST_VALG
  cd $CURDIR
fi
