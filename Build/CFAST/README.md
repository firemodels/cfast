# Compiling CFAST

These folders are used for compiling CFAST with different Fortran compilers (Intel and Gnu), different operating systems (Windows, Linux, MacOS), and mode (db means debug). All folders contain a single bash script called `make_cfast.sh` (or `make_cfast.bat` for Windows) that invokes the same `makefile`. All the build targets are listed in the `makefile`. 

Run the shell or batch script from the directory for the desired build target. Pass `--clean-cfast` to remove object and module files before compiling, for example:

```bash
cd Build/CFAST/gnu_macos
./make_cfast.sh --clean-cfast
```

On Windows, run `make_cfast.bat --clean-cfast` in the selected target directory. The clean option also skips the final interactive pause for automated builds. Intel batch scripts still accept the existing `bot` and `release` arguments alongside `--clean-cfast`.

Without this option, the scripts build incrementally. Use `--clean-cfast` for every verification and bundle build and after changing Fortran module definitions or compiler options, since the makefile does not track all module dependencies. Use `--help` to display script usage.
