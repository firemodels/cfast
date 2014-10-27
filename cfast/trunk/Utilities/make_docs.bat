@echo off
echo Make Technical Reference Guide
cd ..\Docs\Tech_Ref
pdflatex -interaction nonstopmode Tech_Ref > Tech_Ref.err
bibtex Tech_Ref >> Tech_Ref.err
pdflatex -interaction nonstopmode Tech_Ref >> Tech_Ref.err
pdflatex -interaction nonstopmode Tech_Ref >> Tech_Ref.err

echo Make Users Guide
cd ..\Users_Guide
pdflatex -interaction nonstopmode Users_Guide > Users_Guide.err
bibtex Users_Guide >> Users_Guide.err
pdflatex -interaction nonstopmode Users_Guide >> Users_Guide.err
pdflatex -interaction nonstopmode Users_Guide >> Users_Guide.err

echo Make Validation Guide
cd ..\Validation_Guide
pdflatex -interaction nonstopmode Validation_Guide > Validation_Guide.err
bibtex Validation_Guide >> Validation_Guide.err
pdflatex -interaction nonstopmode Validation_Guide >> Validation_Guide.err
pdflatex -interaction nonstopmode Validation_Guide >> Validation_Guide.err
cd ..\..\Utilities

