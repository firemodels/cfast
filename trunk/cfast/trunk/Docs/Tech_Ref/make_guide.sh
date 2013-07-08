#!/bin/bash

# Add LaTeX search path; Paths are ':' separated
export TEXINPUTS=".:../LaTeX_Style_Files:"

clean_build=1

doc=Tech_Ref
# Build guide
pdflatex -interaction nonstopmode $doc &> $doc.err
bibtex $doc &> $doc.err
pdflatex -interaction nonstopmode $doc &> $doc.err
pdflatex -interaction nonstopmode $doc &> $doc.err

# Scan and report any errors in the LaTeX build process
if [[ `grep -E "Error:|Fatal error|! LaTeX Error:" -I $doc.err | grep -v "xpdf supports version 1.5"` == "" ]]
   then
      # Continue along
      :
   else
      echo "LaTeX errors detected:"
      grep -E "Error:|Fatal error|! LaTeX Error:" -I $doc.err | grep -v "xpdf supports version 1.5"
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

if [[ $clean_build == 0 ]]
   then
      :
   else
      echo "$doc buiilt successfully!"
fi    
