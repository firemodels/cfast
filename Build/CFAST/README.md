# Compiling CFAST

These folders are used for compiling CFAST with different Fortran compilers (Intel and Gnu), different operating systems (Windows, Linux, MacOS), and mode (db means debug). All folders contain a single bash script called `make_cfast.sh` (or `make_cfast.bat` for Windows) that invokes the same `makefile`. All the build targets are listed in the `makefile`. 

Run the shell script from the directory for the desired build target. Pass
`--clean-cfast` to remove object and module files before compiling, for example:

```bash
cd Build/CFAST/gnu_macos
./make_cfast.sh --clean-cfast
```

Without this option, the shell scripts build incrementally. Use `--clean-cfast`
after changing Fortran module definitions or compiler options, since the makefile
does not track all module dependencies. Use `--help` to display script usage.
