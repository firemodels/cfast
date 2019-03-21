## Introduction 

To keep CFAST consistent and easier to develop and debug, it is important that we follow some ground rules that will allow us to work together efficiently.  The guidelines below closely align with this article on [Best Practices for Scientific Computing](http://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1001745).

## When Should I Submit a Pull Request?

Short answer: as often as possible!  Commit early and often.  Fail fast.

With that said, we obviously do not want to spend all day, everyday fixing errors caught by the continuous integration system.  So, here are the guidelines for what we would like to see in a pull request (PR):

1. Your code should have verification (unit) tests.  [Here](https://github.com/firemodels/fds/wiki/Verification-Case-Setup-Example) is a guide to setting up a verification test.

2. Your code should have documentation. You can find information on compiling the maunals [here](hhttps://github.com/firemodels/cfast/wiki/Compiling-the-CFAST-Validation-Guide.  Note that once a feature is documented users will expect it to be ready for use in the next release.  Features that are in beta testing should be documented as such.  In some special cases we may opt to maintain a separate document during development (for example, this is being done with Complex Geometry), but it is still important that other developers have access to the documentation.

3. Your code should compile without warnings in debug mode with Intel (if possible) and GNU compilers. You can find information on compiling CFAST [here](https://github.com/firemodels/cfast/wiki/Compiling-CFAST).

4. Your PR should not contain merge commits. The main reason for this rule is that often what we see is that a PR with lots of merges will have mistakes in other areas. If we cannot have a reasonable level of confidence that a binary file has not been accidentally committed, then we cannot merge the PR.  This means it is important to make your PRs reasonably small (just a few commits) and organized.

5. If a large set of commits is unavoidable, it is helpful to get in touch with us ahead of time.  We can then make arrangements to go through the necessary testing of the topic branch before merging to the trunk.

Please note, there may be times, usually close to a minor release, where we will not accept major code or documentation changes.

## Responsibilities of a Developer 

  * Stay up-to-date with the latest code.  Generally, this means pulling changes from the central repository branch [firemodels/master](https://github.com/firemodels/cfast) on a daily basis.  For more instructions on how to setup remote tracking in Git see the [Git Notes Getting Started](https://github.com/firemodels/cfast/wiki/Getting-Started-with-Git) wiki.
  * Update the [theory manual](https://github.com/firemodels/cfast/tree/master/Manuals/Tech_Ref) as necessary.
  * Update the [verification/validation](https://github.com/firemodels/cfast/tree/master/Manuals/Validation_Guide) guides as necessary.
  * Update the [users guide](https://github.com/firemodels/cfast/tree/master/Manuals/Users_Guide) as necessary (hide developer hooks with comments, but do still include them!).  
  * Update [release notes](https://github.com/firemodels/cfast/wiki/Release-Notes).
  * IMPORTANT: Code changes should come with a test case, usually a *verification* test.  **Code modifications that do not have a test case have no guarantee of surviving in future development work.** All test cases must be added to the guides.  Either modify the [script configuration file](https://github.com/firemodels/cfast/blob/master/Utilities/Matlab/CFAST_verification_dataplot_inputs.csv) to process plots for the manuals or add your own script to the [master script](https://github.com/firemodels/cfast/blob/master/Utilities/Matlab/CFAST_verification_script.m).
  * Monitor the [discussion forum](https://groups.google.com/forum/?fromgroups#!forum/cfast) for topics related to your areas of the code.
  * Make use of the GitHub [issues](https://github.com/firemodels/cfast/issues) page to track progress of development work or bugs.
  * A little passion doesn't hurt: We strive to make cfast the best fire code it can be. Our team is dedicated to this goal and works tirelessly in its pursuit.

## Source Code 

  * IMPLICIT NONE (enough said).
  * Use lower case in F90 text files (readability and easy searching). 
  * Use 4 spaces (NO TABS!) for indention of loops, IF statements, etc. (readability, portability across text editors).
  * Do not include excess white space.  You can see the spaces with an editor like [Sublime Text](http://www.sublimetext.com/) or [Atom](https://atom.io/).
  * Double space between subroutines and single space within subroutines (readability).
  * Single space below major block comments (consistency in commenting).
  * Use the Makefile that is in the repository for release and debug versions. *Before committing new source: update your repository just prior to committing and compile in debug mode and make sure there are no errors or warnings*.
  * If an IF block, DO loop, or similar feature is more than a screen length of text, name the section. (helps keep track the start and end of long or complex code structures)
  * Use a named variable rather than a number, for example: `RHO_H2O = 1000._EB` rather than just `1000._EB`  (improves readability)
  * Use integers rather than strings for SELECT CASE, IF blocks, etc. (string comparisons are more costly)
  * Use `>`, `<`, `>=`, `<=`, `==`, and `/=` rather than `.GT.`, `.LT.`, `.GE.`, `.LE.`, `.EQ.`, and `.NE.` Put spaces before and after `.AND.` and `.OR.` but no space between `.NOT.` and the logical variable. (improves readability)
  * Avoid the use of `==` and `/=` for comparing REAL numbers.  Rather than `IF (X == 0._EB)` use `IF (ABS(X) < TWO_EPSILON_EB)`, and rather than `IF (X /= Y)` use `IF(ABS(X-Y) < TWO_EPSILON_EB)`.
  * Explicitly type REAL numbers. Rather than `X = 0.` do `X = 0._EB` (or `_FB` if the appropriate precision is four byte).
  * Use descriptive variable names for improved code readability.  For example, `EXTERNAL_WALL_CELLS` rather than `EWC`.
  * If you hardwire a number into the code, make it a PARAMETER and give some concise reason.
  * If an input is added to a namelist definition, place it alphabetically.  Seeing which inputs have not been documented in the User Manual is much easier when both the manual and the source is alphabetized.
  * Do not use more than 132 characters per line. (line length limit)
  * When using IF statements, put a space before and after the brackets, e.g., `IF (statement) THEN`. (readability)
  * `ENDIF` and `ENDDO` each are one word with no spaces. (consistency)
  * Use `I0` to format integers for output if alignment does not matter.
  * **Put yourself in the shoes of others who have to debug the code.  Treat the code like you are writing prose: rewrite until it is clear.  If someone can't glance at the code and understand what is happening, it needs to be rewritten.**

## Commit Messages

It is helpful is we all use the same conventions for commit messages as this aids in searching the Git logs.  Over the years the following have become standardized among the developers:

* CFAST Source
* CFAST User Guide, CFAST Tech Guide, CFAST Validation Guide, Biliography
* CFAST Validation, CFAST Verification
* Matlab
* [others]

A couple of convenient ways to search the logs are by either `grep` or looking at particular directory.  For example, you could grep on `CFAST Source` commit tag like this
```
$ git log --oneline --no-merges --grep="CFAST Source"
```
Or, you could list commits of files in the `cfast/Source` directory.  Go to the top level of the rep and do
```
$ git log --oneline --no-merges Source
```


## Manuals 

  * The mathematical style of the documents should follow [A. Thompson and B.N. Taylor. The NIST Guide for the Use of the International System of Units. NIST Special Publication 881, 2008](http://www.nist.gov/pml/pubs/sp811/index.cfm).     
  * Remember to add new input parameters to the [FDS User's Guide](https://github.com/firemodels/fds/tree/master/Manuals/FDS_User_Guide). If the parameter is not for public viewing yet, just put a comment character in front of it. This helps us keep track of inputs.
  * Before committing a change to a manual, pdflatex it and make sure nothing is broken.  You should be able to run the `make_guide.sh` script in the guide directory and get the message `<guide> built successfully!`.  If you don't, then cfastbot will get errors or warning messages that someone will have to clean up. 
  * Do not commit the PDF version of a manual to the 'All PDFs' folder in the Repository until we do a release. Sometimes, input parameters get introduced in the guides before an actual release version of the code.
  * Avoid HTML links in the manuals. They too often break.
  * Use conventional (soft) wrapping! This means that there are no hard character returns embedded in your paragraphs. Please help us by reformatting any sections that you find yourself working on. (portability across text editors)
  * Add any custom commands or latex variables to [Bibliography/commoncommands.tex](https://github.com/firemodels/cfast/blob/master/Manuals/Bibliography/commoncommands.tex).
  * Use \textbf, \texttt, etc., instead of \bf, \tt, etc. because [the latter are deprecated](http://www.tex.ac.uk/cgi-bin/texfaq2html?label=2letterfontcmd).


## Verification and Validation Suites 

  * Read the wiki pages that describes the process of compiling the V&V Guides. Make sure that you can compile the Guides before making any changes.
  * File naming convention: try not to use a hyphen (causes problems with tab completion in Cygwin).
