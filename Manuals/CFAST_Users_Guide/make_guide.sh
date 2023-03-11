#!/bin/bash

# Add LaTeX search path; Paths are ':' separated
export TEXINPUTS=".:../LaTeX_Style_Files:"

clean_build=1

doc=CFAST_Users_Guide
docpdf=${doc}.pdf
rm -f $docpdf
# Build guide
gitrevision=`git describe --long --dirty`
echo "\\newcommand{\\gitrevision}{$gitrevision}" > ../Bibliography/gitrevision.tex
pdflatex -interaction nonstopmode $doc &> $doc.err
bibtex $doc &> $doc.err
pdflatex -interaction nonstopmode $doc &> $doc.err
pdflatex -interaction nonstopmode $doc &> $doc.err

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
   else
      echo "$doc build succeeded"
fi    

rm -f ../Bibliography/gitrevision.tex
