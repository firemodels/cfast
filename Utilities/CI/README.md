# CFAST continuous integration

CFAST owns the build, Verification/Validation, CEditQt, plotting, manual, and result-processing pipeline in this directory. The single-case launcher is `../scripts/qcfast.sh`. The case lists are in `Verification/scripts` and `Validation/scripts`. These scripts run from any working directory.

## Run a suite

From the CFAST repository root:

```bash
Utilities/CI/Run_CFAST_Cases.sh --suite Verification -I gnu -q batch
Utilities/CI/Run_CFAST_Cases.sh --suite Validation -I intel -q batch
Utilities/CI/Run_CFAST_Cases.sh --suite Verification --test-UI -q batch
```

Use `-d` for debug, `-m 2` to stop after two iterations, `-s` to request stops, `-t` to report saved timings, `--case suppression.in` to select a case, and `-v` to print job scripts without submitting them. `-e` selects an explicit executable. The `Verification/scripts/Run_CFAST_Cases.sh` and Validation counterpart forward to this shared runner.

Suite execution is local by default; specify `-q partition` to submit to Slurm. Cases run sequentially, write the same per-case logs and exit files, and stop the suite on the first failure. For example, from `Verification/scripts`, run `./Run_CFAST_Cases.sh -e ../../Build/CFAST/gnu_macos/cfast8_macos`, or use `-I gnu` to select the standard GNU build for your platform. Executable paths are resolved relative to the caller's working directory; executable names on `PATH` also work, but interactive shell aliases do not. The single-case launcher uses the same rule: local execution unless `-q partition` is supplied.

The launcher uses Slurm for batch jobs and records a job manifest when `CFAST_JOB_MANIFEST` is set. Each completed case writes an `.exit` file. UI tests use separate `.ui.log`, `.ui.err`, `.ui.slog`, and `.ui.exit` files. The pipeline queries Slurm for the suite together and reports completed, running, pending, confirming, and failed counts. The summary updates when counts change and at least every 30 seconds while waiting; confirming means a job has left the queue and its completion record has not appeared yet. Individual case details are printed for failures. If a Slurm job has no `.exit` file, it checks `sacct` for the exact job ID and accepts only `COMPLETED` with exit code `0:0`. Accounting is retried briefly to allow for reporting delays. Failed jobs and jobs whose success cannot be confirmed stop the pipeline. UI jobs import and rewrite inputs; they do not run CFAST again.

## CEditQt editor and units checks

With `--test-UI`, every selected Verification/Validation case is imported and rewritten. General editor checks run once, in the first selected case of each suite; later cases use `--skip-editor-checks`. Failures in either the editor checks or a case rewrite fail that job and the CI stage. The first case's `.ui.log` reports individual editor checks and the units coverage summary.

Run the same general checks directly, without CFAST or a suite copy:

```bash
python Source/CeditQt/run_verification_ui_tests.py --editor-checks-only
```

The general checks invoke `Verification/Units/check_ceditqt_units.py --ui`. That checker uses the three Units inputs and their expected-value CSV, independent numerical reference data for every supported quantity and dependent unit combination, and Qt checks across all editor tabs and custom dialogs. It exercises all selector choices, SI/English resets, mixed profiles, conditional setpoints, table displays, plots, import dialogs, run status, and input-file round trips. Inventory assertions require coverage updates when quantities, unit choices, custom dialogs, tabs, unit fields, or table columns are added. The tests use temporary inputs/settings and do not run simulations. Qt and Matplotlib must be installed; the runner selects the offscreen Qt backend.

To run just the units checks:

```bash
python Verification/Units/check_ceditqt_units.py --ui
```

## Run the complete pipeline on existing checkouts

Run from `cfast/Utilities/CI` in the prepared repository collection:

```bash
./run_cfastbot.sh -q firebot -U -m your@email.address
```

Cases run locally and sequentially by default; `-q` selects a Slurm partition, `-U` enables uploads, and `-m` sets the notification email. `-h` displays help. When `-m` is omitted, `cfastbot_email_list.sh` supplies the recipient if configured. Linux also falls back to the configured Git email. On macOS, an unconfigured recipient leaves the report in `email_body.txt` without requiring email delivery.

The launcher finds sibling `fds`, `exp`, and `smv` repositories from its own location. Every invocation runs the complete pipeline: clean CFAST builds (GNU on macOS, Intel on Linux), Smokeview builds (Clang on macOS, Intel on Linux), Verification/Validation, CEditQt checks, figures, and manuals. Each V&V case gets a short debug initialization run (two iterations) and one full release simulation. The separate UI check imports and saves the input without running CFAST; its rewritten inputs and logs do not replace the simulation CSV files used for plotting. It uses Python and LaTeX; Slurm is required only when `-q` is supplied. Standalone suite execution does not require the Smokeview or manual toolchains.

On a standalone Mac, run `./run_cfastbot.sh` with no queue option. Install GNU Fortran, the Xcode command-line tools, and a LaTeX distribution providing `pdflatex` and `biber`. Smokeview uses its native Clang/Cocoa build. Run from a graphical desktop session for figure rendering. To reuse a provisioned Python environment, set `CFAST_CI_PYTHON` to the absolute path of its Python executable. The normal run uses the existing checkouts; repository preparation is a separate operation.

On Windows, use a compiler-enabled PowerShell terminal from `cfast/Utilities/CI`:

```powershell
.\run_cfastbot.ps1
.\run_cfastbot.ps1 -Compiler gnu -Python C:\path\to\python.exe
```

The Windows launcher runs the full pipeline locally and sequentially, without Bash or Slurm. It requires GNU Make, Intel C/C++ for Smokeview, Intel Fortran by default (or GNU Fortran with `-Compiler gnu`), Python with the FDS plotting and CEditQt dependencies, and `pdflatex`/`biber`. Activate the compiler environment before launching. Python selection uses `CFAST_CI_PYTHON`, then an explicit `-Python`, then `fds/.github/fds_python_env/Scripts/python.exe`. If none is supplied or available, the launcher provisions `cfast/.github/cfast_python_env` from the mirrored CFAST requirements. Use `-StateDirectory` to override the external log directory. Stage logs, the final `exit_code`, validation statistics, and successful guide PDFs are saved under the run directory. Per-case `.stdout`, `.err`, and `.exit` files remain beside the inputs; UI checks use a separate `.ui` suffix. The Windows launcher saves results locally and does not send email or publish bundles.

Progress and command output are printed to stderr as the run proceeds and saved in the stage logs. The launcher prints the log directory at startup. Batch jobs report submission and suite progress on screen; individual case output remains in the case log files. CFAST writes its console output to each case's `.err` file and its model log to `.log`. Follow a running case from another terminal, for example:

```bash
tail -f ../../Verification/Energy_Balance/sealed_test.err
```


## Shared repository preparation

Keep the dedicated CI repositories under a common directory, such as `/home/firebot/FireModels_cbot`. Copy `cfast/Utilities/CI/update_repos.sh` to that directory before running it, so the running script stays outside the checkout it updates. It removes stale Git locks, fetches tags and full history, replaces local branches with their remote branches, and discards local commits, edits, and untracked/ignored files. Submodules are reset to the commits recorded by their parent repositories. CFASTbot uses the prepared revisions and performs no Git updates or cleanup.

Run preparation and CFASTbot in sequence, so a preparation failure prevents CI:

```bash
cd /home/firebot/FireModels_cbot &&
cp cfast/Utilities/CI/update_repos.sh . &&
./update_repos.sh && ./cfast/Utilities/CI/run_cfastbot.sh -q firebot -U
```

Run preparation only while no other jobs are using these checkouts. Complete repository preparation before starting a bot, and let the bot finish before preparing repositories again.

A cron entry that copies the script, prepares the repositories, and then runs CFASTbot is:

```cron
1 3 * * * /bin/bash -lc '(cd /home/firebot/FireModels_cbot && cp cfast/Utilities/CI/update_repos.sh . && ./update_repos.sh && ./cfast/Utilities/CI/run_cfastbot.sh -q firebot -U) > /home/firebot/.cfastbot/cfastbot_daily.out 2>&1'
```

Create `/home/firebot/.cfastbot` before using that log redirection. The log captures both preparation and CI output; `&&` prevents CI from starting if preparation fails. CFASTbot needs no workspace initialization or repository configuration file.

## State and Python

State defaults to `$HOME/.cfastbot`; override it with `CFAST_CI_STATE_DIR`. It must be outside the cleaned checkouts. It contains per-run logs under `runs/` and history. Python environments and the Matplotlib cache live in the repository collection so compute nodes can access them. `latest_run` records the most recent log directory. Each run records repository and submodule revisions and its final exit code. Successful runs save PDFs and revision metadata in `VERSION`; `VERSION_LATEST` contains metadata for the current run.

On Linux and macOS, CI uses `CFAST_CI_PYTHON` when supplied, otherwise the existing sibling `fds/.github/fds_python_env/bin/python`. Only if that FDS interpreter is absent does it run `Utilities/Python/setup_python_env.sh --batchmode` to provision or reuse `cfast/.github/cfast_python_env`. Windows follows the same selection with `Scripts/python.exe` and the PowerShell setup script. An existing FDS environment is checked for required imports but is never rebuilt or updated by CFAST CI; a failed import stops the run and reports the selected interpreter.

CFAST's `.github/requirements.txt` mirrors FDS's package list, with the editable `fdsplotlib` path adjusted to the sibling FDS checkout. Keep these lists synchronized when dependencies change. CFAST setup reuses its environment and installs changed requirements in place. The Matplotlib cache is `cfast/.github/cfast_matplotlib`. With Slurm, the selected interpreter, repository collection, and cache must be accessible from compute nodes. Repository preparation deletes ignored files, including these environments; selection occurs afterward. Linux bundle builds also require PyInstaller, which is included in the mirrored requirements.

## Bundles and publishing

CFAST platform bundles are built with the scripts in [`Build/bundle`](../../Build/bundle/README.md). On Linux, `-U` builds CEditQt and calls `Build/bundle/build_linux_bundle.sh` directly to package the tested CFAST and Smokeview executables and the generated manuals. Repository updates and CFAST, Smokeview, and manual rebuilds are disabled during this packaging step. The tarball and staging files are stored in the run's log directory.

`-U` uploads the manuals, revision information, and Linux tarball to the GitHub release selected by `GH_OWNER`, `GH_REPO`, and `GH_CFAST_TAG`. GitHub CLI (`gh`) must be installed and authenticated. The macOS and Windows platform scripts can use these manuals with `--manuals-from-release`.
