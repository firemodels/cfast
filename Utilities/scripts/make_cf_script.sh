#/bin/bash
BEG=$1
END=$2
BASE=Tam_8_AH_A-
EXT=.in

export STOPFDSMAXITER=100000
cat << EOF
#!/bin/bash
CFAST=$FIREMODELS/cfast/Build/CFAST/intel_linux_64/cfast7_linux_64

EOF

for i in `seq $BEG $END`; do
  echo \$CFAST $BASE$i$EXT -V
done
