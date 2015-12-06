#!/bin/bash
CCnotfound=
USEINSTALL=y

if [[ $CCnotfound -eq 1 ]] && [[ "$USEINSTALL" == "" ]]; then
  USEINSTALL=
  USEINSTALL2=
else
  USEINSTALL="-i"
  USEINSTALL2="-u"
fi
echo USEINSTALL=$USEINSTALL
