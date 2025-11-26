#!/bin/bash

OS=_linux
bundlebase=
cfastrepo=cfast
fdsrepo=FDS-SMV
firelocalrepo=~/FIRE-LOCAL
uploaddir=

if [ "$PLATFORM" == "LINUX64"]; then
   ostype=LINUX
fi
if [ "$PLATFORM" == "OSX64"]; then
   ostype=OSX
fi

while getopts 'b:c:f:F:o:s:u:' OPTION
do
case $OPTION  in
  b)
   bundlebase="$OPTARG"
   ;;
  c)
   cfastrepo="$OPTARG"
   ;;
  f)
   fdsrepo="$OPTARG"
   ;;
  F)
   firelocalrepo="$OPTARG"
   ;;
  o)
   OS="$OPTARG"
   ;;
  s)
   size="$OPTARG"
   ;;
  u)
   uploaddir="$OPTARG"
   ;;
esac
done
shift $(($OPTIND-1))

if [ "$bundlebase" == "" ]; then
  echo "The -b bundlebase parameter is required"
  echo "bundle_generic.sh aborted"
  exit
fi
if [ "$uploaddir" == "" ]; then
  echo "The -u uploaddir parameter is required"
  echo "bundle_generic.sh aborted"
  exit
fi


# this script is called by bundle_platform.sh
# where platform may be linux or osx

smokeviewdir=intel$OS
smokeview=smokeview$OS
smokeviewout=smokeview$OS

backgrounddir=intel$OS
background=background
backgroundout=background

cfastdir=intel$OS
cfast=cfast7_intel$OS
cfastout=cfast7$OS

scp_fds_smvroot=\~$fdsrepo
fds_smvroot=$fdsrepo

scp_cfast_smvroot=\~$cfastrepo
cfast_smvroot=$cfastrepo
cfastroot=$scp_cfast_smvroot/CFAST

backgroundroot=$scp_fds_smvroot/Utilities/background

bundledir=$bundlebase

mandir=$firelocalrepo/reports/fds_manuals
smvbindir=$scp_fds_smvroot/SMV/Build/$smokeviewdir
forbundle=$fds_smvroot/SMV/for_bundle
texturedir=$forbundle/textures

makeinstaller=$cfast_smvroot/Utilities/Scripts/make_installer.sh

cd $uploaddir
rm -rf $bundlebase
mkdir $bundledir
mkdir $bundledir/bin
mkdir $bundledir/Documentation
mkdir $bundledir/Examples
mkdir $bundledir/bin/textures

echo Copying program files

# smokeview

echo copying $smokeview from $smvbindir on $smvhost
scp -q $smvhost\:$smvbindir/$smokeview $bundledir/bin/$smokeviewout

echo copying textures
cp $texturedir/*.png $bundledir/bin/textures/.
cp $texturedir/*.jpg $bundledir/bin/textures/.

echo copying smokeview.ini from $forbundle
cp $forbundle/smokeview.ini $bundledir/bin/.

echo copying volrender.ssf from $forbundle
cp $forbundle/volrender.ssf $bundledir/bin/.

echo copying objects.svo from $forbundle
cp $forbundle/objects.svo $bundledir/bin/objects.svo

# CFAST 

echo copying $cfast from $cfastdir on $cfasthost
scp -q $cfasthost\:$cfastroot/$cfastdir/$cfast $bundledir/bin/$cfastout

# background

echo copying $background from $backgrounddir on $cfasthost
scp -q $cfasthost\:$backgroundroot/$backgrounddir/$background $bundledir/bin/$backgroundout


# put in shell script lines to get documentation
echo Copying documentation
cp $mandir/SMV_User_Guide.pdf $bundledir/Documentation/.
cp $mandir/SMV_Technical_Reference_Guide.pdf $bundledir/Documentation/.
cp $mandir/SMV_Verification_Guide.pdf $bundledir/Documentation/.

if [ x"$INTELLIB" != "x"]; then
  echo copying  run time libraries
  cp -r $INTELLIB $bundledir/bin/$DESTLIB
fi

# put in shell script lines to get release notes
#.....

# put in shell script lines to get cfast exmamples
#.....

cd $curdir

echo Building archive
rm -rf $uploaddir/$bundlebase.tar
rm -rf $uploaddir/$bundlebase.tar.gz
cd $uploaddir/$bundlebase
tar cf ../$bundlebase.tar --exclude='*.csv' .
echo Compressing archive
gzip    ../$bundlebase.tar
echo Creating installer
cd ..
$makeinstaller -o $ostype -i $bundlebase.tar.gz -d $INSTALLDIR $bundlebase.sh 
