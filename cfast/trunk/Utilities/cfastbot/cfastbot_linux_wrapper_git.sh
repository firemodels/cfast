#!/bin/bash

running=~/cfastbot/cfastbot_running
if [ -e $running ] ; then
  exit
fi
touch $running
cd ~/cfastbot
svn update
./cfastbot_linux_git.sh "$@"
rm $running

