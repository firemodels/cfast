#!/bin/csh -f

set directory=$1
set      host=$2

echo Updating the GIT repository $directory on $host to the latest revision
echo
ssh -q $host \( cd \~/$directory \; git remote update \; git merge origin/master \)
