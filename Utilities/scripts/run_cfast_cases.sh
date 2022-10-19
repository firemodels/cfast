#!/bin/bash
QCFAST="$FIREMODELS/cfast/Utilities/scripts/qcfast.sh -q batch2"

BEG=1
END=20
for i in `seq 1 50`; do
  SCRIPT=group$i
  ./make_cf_script.sh $BEG $END > $SCRIPT
  $QCFAST -S $SCRIPT
  BEG=`expr $BEG + 20`
  END=`expr $END + 20`
done

