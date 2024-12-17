#!/bin/bash

if [ $# -lt 1 ]
then
  echo "Usage: make_installer.sh -o ostype -i CFAST_TAR.tar.gz -d installdir INSTALLER.sh"
  echo ""
  echo "Creates a cfast installer sh script. "
  echo ""
  echo "  -o ostype - OSX or LINUX"
  echo "  -i CFAST.tar.gz - compressed tar file containing CFAST distribution"
  echo "  -d installdir - default install directory"
  echo "   INSTALLER.sh - bash shell script containing self-extracting CFAST installer"
  echo
  exit
fi

INSTALLDIR=
CFAST_TAR=
ostype=
INSTALLER=

while getopts 'd:i:o:' OPTION
do
case $OPTION in
  d)
  INSTALLDIR="$OPTARG"
  ;;
  i)
  CFAST_TAR="$OPTARG"
  ;;
  o)
  ostype="$OPTARG"
  ;;
esac
done 
shift $(($OPTIND-1))

INSTALLER=$1

if [ "$ostype" == "" ]
then
echo "*** fatal error: OS type (OSX or LINUX) not specified"
exit 0
fi

if [ "$CFAST_TAR" == "" ]
then
echo "*** fatal error: CFAST distribution file not specified"
exit 0
fi

if [ "$INSTALLDIR" == "" ]
then
echo "*** fatal error: default install directory not specified"
exit 0
fi

if [ "$INSTALLER" == "" ]
then
echo "*** fatal error: installer not specified"
exit 0
fi

LDLIBPATH=LD_LIBRARY_PATH
if [ "$ostype" == "OSX" ]
then
LDLIBPATH=DYLD_LIBRARY_PATH
fi

size2=64

ostype2=$ostype
if [ "$ostype" == "LINUX" ]
then
ostype2=Linux
fi

cat << EOF > $INSTALLER
#!/bin/bash

OVERRIDE=\$1
echo ""
echo "Installing $size2 bit $ostype2 CFAST $CFASTVERSION and Smokeview $SMVVERSION"
echo ""
echo "Options:"
echo "  1) Press <Enter> to begin installation"
echo "  2) Type \"extract\" to copy the installation files to $CFAST_TAR"

BAK=_\`date +%Y%m%d_%H%M%S\`

#--- make a backup of a file

BACKUP_FILE()
{
  INFILE=\$1
  if [ -e \$INFILE ]
  then
  echo
  echo Backing up \$INFILE to \$INFILE\$BAK
  cp \$INFILE \$INFILE\$BAK
fi
}

#--- convert a path to it absolute equivalent

function ABSPATH() {
  pushd . > /dev/null;
  if [ -d "\$1" ];
  then
    cd "\$1";
    dirs -l +0;
  else
    cd "\`dirname \"\$1\"\`";
    cur_dir=\`dirs -l +0\`;
    if [ "\$cur_dir" == "/" ]; then
      echo "\$cur_dir\`basename \"\$1\"\`";
    else
      echo "\$cur_dir/\`basename \"\$1\"\`";
    fi;
  fi;
  popd > /dev/null;
}

#--- make a directory, checking if the user has permission to create it

MKDIR()
{
  DIR=\$1
  CHECK=\$2
  if [ ! -d \$DIR ]
  then
    echo "Creating directory \$DIR"
    mkdir -p \$DIR>&/dev/null
  else
    if [ "\$CHECK" == "1" ] 
    then
      while true; do
          echo "The directory, \$DIR, already exists."
          if [ "\$OVERRIDE" == "y" ]
            then
              yn="y"
          else
              read -p "Do you wish to overwrite it? (yes/no) " yn
          fi
          case \$yn in
              [Yy]* ) break;;
              [Nn]* ) echo "Installation cancelled";exit;;
              * ) echo "Please answer yes or no.";;
          esac
      done
      rm -rf \$DIR>&/dev/null
      mkdir -p \$DIR>&/dev/null
    fi
  fi
  if [ ! -d \$DIR ]
  then
    echo "Creation of \$DIR failed.  Likely cause,"
    echo "\`whoami\` does not have permission to create \$DIR."
    echo "CFAST installation aborted."
    exit 0
  else
    echo The installation directory, "\$DIR, has been created."
  fi
  touch \$DIR/temp.\$\$>&/dev/null
  if ! [ -e \$DIR/temp.\$\$ ]
  then
    echo "\`whoami\` does not have permission to write to \$DIR"
    echo "CFAST installation aborted."
    exit 0
  fi
  rm \$DIR/temp.\$\$
}

#--- record the name of this script and the name of the directory 
#    it will run in

THISSCRIPT=\`ABSPATH \$0\`
THISDIR=\`pwd\`

#--- record temporary startup file names

BASHCFAST=/tmp/bashrc_cfast.\$\$

#--- Find the beginning of the included CFAST tar file so that it 
#    can be subsequently un-tar'd
 
SKIP=\`awk '/^__TARFILE_FOLLOWS__/ { print NR + 1; exit 0; }' \$0\`

#--- extract tar.gz file from this script if 'extract' specified

if [ "\$OVERRIDE" == "y" ] 
then
  option=""
else
  read  option
fi

if [ "\$option" == "extract" ]
then
  name=\$0
  THAT=$CFAST_TAR
  if [ -e \$THAT ]
  then
    while true; do
      echo "The file, \$THAT, already exists."
      read -p "Do you wish to overwrite it? (yes/no) " yn
      case \$yn in
        [Yy]* ) break;;
        [Nn]* ) echo "Extraction cancelled";exit;;
        * ) echo "Please answer yes or no.";;
      esac
    done
  fi
  echo Extracting the file embedded in this installer to \$THAT
  tail -n +\$SKIP \$THISSCRIPT > \$THAT
  exit 0
fi

OSSIZE=\`getconf LONG_BIT\`
if [ "\$OSSIZE" != "64" ] ; then
  if [ "\$OSSIZE" == "32" ] ; then
    echo "***Fatal error: CFAST and Smokeview require a 64 bit operating system."
    echo "   The size of the operating system found is \$OSSIZE."
    exit 0
  fi
  echo "***Warning: CFAST and Smokeview require a 64 bit operating system."
  echo "   The size of the operating system found is \$OSSIZE."
  echo "   Proceed with caution."
fi

#--- get CFAST root directory

echo ""
echo "Where would you like to install CFAST?"
EOF

if [ "$ostype" == "OSX" ]
then
cat << EOF >> $INSTALLER
    echo "  Press 1 to install in /Applications/$INSTALLDIR"
    echo "  Press 2 to install in \$HOME/$INSTALLDIR"
EOF
  else
cat << EOF >> $INSTALLER
    echo "  Press 1 to install in \$HOME/$INSTALLDIR"
    echo "  Press 2 to install in /opt/$INSTALLDIR"
    echo "  Press 3 to install in /usr/local/bin/$INSTALLDIR"
EOF
  fi
cat << EOF >> $INSTALLER
echo "  Enter a directory path to install elsewhere"

if [ "\$OVERRIDE" == "y" ] 
then
  answer="1"
else
  read answer
fi
EOF

if [ "$ostype" == "OSX" ]
then
cat << EOF >> $INSTALLER
  if [[ "\$answer" == "1" || "\$answer" == "" ]]; then
    eval CFAST_root=/Applications/$INSTALLDIR
  elif [[ "\$answer" == "2" ]]; then
    eval CFAST_root=\$HOME/$INSTALLDIR
  else
    eval CFAST_root=\$answer
  fi
EOF
else
cat << EOF >> $INSTALLER
  if [[ "\$answer" == "1" || "\$answer" == "" ]]; then
    eval CFAST_root=\$HOME/$INSTALLDIR
  elif [ "\$answer" == "2" ]; then
    CFAST_root=/opt/$INSTALLDIR
  elif [ "\$answer" == "3" ]; then
    CFAST_root=/usr/local/bin/$INSTALLDIR
  else
    eval CFAST_root=\$answer
  fi
EOF
fi

cat << EOF >> $INSTALLER

#--- do we want to proceed

while true; do
   echo ""
   echo "Installation directory: \$CFAST_root"
   if [ "\$OVERRIDE" == "y" ] ; then
     yn="y"
   else
     read -p "Do you wish to proceed with the installation? (yes/no) " yn
   fi
   case \$yn in
      [Yy]* ) break;;
      [Nn]* ) echo "Installation cancelled";exit;;
      * ) echo "Please answer yes or no.";;
   esac
done
 
#--- make the CFAST root directory

echo ""
echo "Installation beginning"
 
MKDIR \$CFAST_root 1

#--- copy installation files into the CFAST_root directory

echo
echo "Copying CFAST installation files to"  \$CFAST_root
cd \$CFAST_root
tail -n +\$SKIP \$THISSCRIPT | tar -xz
echo "Copy complete."

#--- create BASH startup file

cat << BASH > \$BASHCFAST
#/bin/bash

# CFAST location

CFASTBINDIR=\`pwd\`/bin

# Update $LDLIBPATH and PATH

export PATH=\\\$CFASTBINDIR:\\\$PATH

BASH

#--- create .bash_cfast startup file

BACKUP_FILE ~/.bashrc_cfast

if [ -e ~/.bashrc_cfast ] ; then
  echo Updating .bashrc_cfast
else
  echo Creating .bashrc_cfast
fi
cp \$BASHCFAST ~/.bashrc_cfast
rm \$BASHCFAST

#--- update .bash_profile
EOF
if [ "$ostype" == "OSX" ]; then
cat << EOF >> $INSTALLER
  BACKUP_FILE ~/.bash_profile

  BASHSTARTUP=/tmp/.bash_profile_temp_\$\$
  cd \$THISDIR
  echo "Updating .bash_profile"
  grep -v bashrc_cfast ~/.bash_profile | grep -v "#CFAST"  > \$BASHSTARTUP
  echo "#CFAST environment -----------------------------" >> \$BASHSTARTUP
  echo "source ~/.bashrc_cfast"                           >> \$BASHSTARTUP
  echo "#CFAST --------------------------------------------" >> \$BASHSTARTUP
  cp \$BASHSTARTUP ~/.bash_profile
  rm \$BASHSTARTUP
EOF
else
cat << EOF >> $INSTALLER
#--- update .bashrc
  BACKUP_FILE ~/.bashrc

  BASHSTARTUP=/tmp/.bashrc_temp_\$\$
  cd \$THISDIR
  echo "Updating .bashrc"
  grep -v bashrc_cfast ~/.bashrc | grep -v "#CFAST" > \$BASHSTARTUP
  echo "#CFAST environment -----------------------" >> \$BASHSTARTUP
  echo "source ~/.bashrc_cfast"                     >> \$BASHSTARTUP
  echo "#CFAST -----------------------------------" >> \$BASHSTARTUP
  cp \$BASHSTARTUP ~/.bashrc
  rm \$BASHSTARTUP
EOF
fi

cat << EOF >> $INSTALLER

echo ""
echo "Installation complete."
exit 0


__TARFILE_FOLLOWS__
EOF
chmod +x $INSTALLER
cat $CFAST_TAR >> $INSTALLER
echo "Installer created."
