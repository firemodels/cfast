#/bin/bash
BASE=$1
BEG=$2
END=$3
OUTFILE=$4
EXT=.in

export STOPFDSMAXITER=100000
cat << EOF > $OUTFILE
#!/bin/bash
CFAST=$FIREMODELS/cfast/Build/CFAST/intel_linux_64/cfast7_linux_64

EOF

rm -f error
for i in `seq $BEG $END`; do
  INPUT=$BASE$i$EXT
  if [ ! -e $INPUT ]; then
     echo "***error: the input file $INPUT does not exist"
     error=1
  fi
  echo \$CFAST $INPUT -V >> $OUTFILE
done
if [  "$error" != "" ]; then
  touch error
fi
