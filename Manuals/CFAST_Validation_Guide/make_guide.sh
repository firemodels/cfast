#!/bin/bash

set -e

# Add LaTeX search path; Paths are ':' separated
export TEXINPUTS=".:../LaTeX_Style_Files:"

clean_build=1

doc=CFAST_Validation_Guide
docpdf=${doc}.pdf
rm -f $docpdf
# Build guide
gitrevision=`git describe --long --dirty`
echo "\\newcommand{\\gitrevision}{$gitrevision}" > ../Bibliography/gitrevision.tex
trap 'rm -f ../Bibliography/gitrevision.tex' EXIT
pdflatex -interaction nonstopmode $doc &> $doc.err
biber $doc &> $doc.err

# Bibliography changes can move the appendices. Continue until the references,
# contents, and figure/table lists all agree with the final pagination.
references_stable=0
for pass in {1..6}; do
  before=$(cksum *.aux "$doc.toc" "$doc.lof" "$doc.lot")
  echo "Building $doc: reference pass $pass"
  pdflatex -interaction nonstopmode $doc &> $doc.err
  after=$(cksum *.aux "$doc.toc" "$doc.lof" "$doc.lot")
  if [ "$before" = "$after" ]; then
    references_stable=1
    break
  fi
done
if [ "$references_stable" = 0 ]; then
  echo "$doc references did not stabilize after six passes" >&2
  exit 1
fi

# Scan and report any errors in the LaTeX build process
if [[ `grep -E "Error:|Fatal error|! LaTeX Error:|Paragraph ended before|Missing \\\$ inserted|Misplaced" -I $doc.err | grep -v "xpdf supports version 1.5"` == "" ]]
   then
      # Continue along
      :
   else
      echo "LaTeX errors detected:"
      grep -E "Error:|Fatal error|! LaTeX Error:|Paragraph ended before|Missing \\\$ inserted|Misplaced" -I $doc.err | grep -v "xpdf supports version 1.5"
      clean_build=0
fi

# Check for LaTeX warnings (undefined references or duplicate labels)
if [[ `grep -E "undefined|multiply defined|multiply-defined" -I $doc.err` == "" ]]
   then
      # Continue along
      :
   else
      echo "LaTeX warnings detected:"
      grep -E "undefined|multiply defined|multiply-defined" -I $doc.err
      clean_build=0
fi

if [ ! -e $docpdf ]; then
  clean_build=0
fi
if [[ $clean_build == 0 ]]; then
      echo "$doc build failed"
      exit 1
   else
      echo "$doc build succeeded"
fi    
