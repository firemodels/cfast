# Running CFAST Verification cases

From this directory, run the suite locally with an existing CFAST executable:

```bash
./Run_CFAST_Cases.sh -e ../../Build/CFAST/gnu_macos/cfast8_macos
```

By default, the runner executes cases sequentially on this machine without Slurm and stops on the first failed case. Each case writes `.log`, `.err`, `.slog`, and `.exit` files next to its input. Add `--case sealed_test.in` to run one case or `-v` to preview commands without running them.

`-e` accepts an absolute path, a path relative to your current directory, or an executable name on `PATH`. Interactive shell aliases such as `cfast` are not inherited by the scripts; pass the executable path behind the alias. With the standard build layout, `./Run_CFAST_Cases.sh -I gnu` selects the GNU executable for your platform automatically; add `-d` for the debug build.

To submit to Slurm, specify `-q partition`. The shared suite runner is `Utilities/CI/Run_CFAST_Cases.sh`. From the CFAST root:

```bash
Utilities/CI/Run_CFAST_Cases.sh --suite Verification -q batch
Utilities/CI/Run_CFAST_Cases.sh --suite Verification -I gnu -q batch
```

Use `--test-UI` to import and rewrite the inputs through CEditQt without running CFAST. The `Run_CFAST_Cases.sh` in this directory calls the shared runner. See [the CI documentation](../../Utilities/CI/README.md) for compiler, debug, queue, stop, timing, and case-selection options.

## Windows

If PowerShell blocks direct execution of `.ps1` files, use the `.bat` launcher from this directory in either Command Prompt or PowerShell:

```bat
.\Run_CFAST_Cases.bat -e "C:\My Builds\cfast8_win.exe"
.\Run_CFAST_Cases.bat -d -I gnu
```

The batch launcher passes all options to the adjacent PowerShell script and returns its exit code. It uses Windows PowerShell with `-ExecutionPolicy Bypass` for that process only; it does not change the saved execution policy or require administrator privileges. An execution policy enforced by organization Group Policy can still block it. Use double quotes around paths containing spaces when calling the batch launcher.

You can also run the native PowerShell script directly from this directory:

```powershell
.\Run_CFAST_Cases.ps1 -e ..\..\Build\CFAST\gnu_win\cfast8_win.exe
.\Run_CFAST_Cases.ps1 -d -I gnu
.\Run_CFAST_Cases.ps1 -e 'C:\My Builds\cfast8_win.exe' -Case sealed_test.in -v
```

This script reads the entries in `CFAST_Cases.sh` and runs CFAST directly. It requires only PowerShell and CFAST; Git Bash and the CI runner are not used. Cases run sequentially in list order and stop on the first failure. Console output is captured in `.stdout` and `.err` files, and an `.exit` file records each case's exit code. CFAST writes its own model `.log` file. These files are stored next to the input; existing stop files are cleared before each run. All selected inputs are checked before the first case starts.

`-e` accepts an absolute path, a path relative to your current directory, or an executable name on `PATH`. Quote paths containing spaces. Without `-e`, the script selects the standard Intel Windows build; use `-I gnu` for GNU and add `-d` for debug. Use `-Case` to select a filename or stem, `-v` to preview without changing outputs, and `-h` for help. The PowerShell runner supports the simple `$RUNCFAST -d directory input.in` entries in the verification list and reports an error if it encounters other shell syntax.
