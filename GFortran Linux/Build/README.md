# Compiling CFAST

The folders above are used for compiling CFAST with different Fortran compilers (Intel, Gnu, PGI), different operating systems (Windows, Linux, Mac OS X), and mode (db means debug, dv means low level of optimization, nothing indicates release version). All folders contain a single bash script called `make_cfast.sh` (or `make_cfast.bat` for Windows) that invokes the same `makefile`. All the build targets are listed in the `makefile`. 

If you find a particular set of options that is no longer appropriate, let us know, preferably via a pull request for the makefile.
