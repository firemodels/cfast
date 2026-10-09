# CFAST continuous integration

CFAST owns the build, Verification/Validation, CEditQt, plotting, manual, and result-processing pipeline in this directory. The single-case launcher is `../scripts/qcfast.sh`. The case lists are in `Verification/scripts` and `Validation/scripts`. These scripts run from any working directory.

## Run a suite

From the CFAST repository root:

```bash
Utilities/CI/Run_CFAST_Cases.sh --suite Verification -I gnu -q terminal
Utilities/CI/Run_CFAST_Cases.sh --suite Validation -I intel -q batch
Utilities/CI/Run_CFAST_Cases.sh --suite Verification --test-UI -q batch
```

Use `-d` for debug, `-m 2` to stop after two iterations, `-s` to request stops, `-t` to report saved timings, `--case e_coefficient.in` to select a case, and `-v` to print job scripts without submitting them. `-e` selects an explicit executable. `-q none` runs cases locally in the background. The `Verification/scripts/Run_CFAST_Cases.sh` and Validation counterpart forward to this shared runner.

The launcher supports Slurm and PBS/Torque and records a job manifest when `CFAST_JOB_MANIFEST` is set. Each completed case writes an `.exit` file. UI tests use separate `.ui.log`, `.ui.err`, `.ui.slog`, and `.ui.exit` files. The pipeline waits for its own jobs and fails when a job exits unsuccessfully or disappears without a completion record. UI jobs import and rewrite inputs; they do not run CFAST again.

## Run the complete pipeline on existing checkouts

Run from `cfast/Utilities/CI` in the prepared repository collection:

```bash
./run_cfastbot.sh -q firebot -U -m your@email.address
```

`-q` selects the scheduler queue (`terminal` runs locally), `-U` enables uploads, and `-m` sets the notification email. `-h` displays help. When `-m` is omitted, `cfastbot_email_list.sh` or the configured Git email supplies the recipient.

The launcher finds sibling `fds`, `exp`, and `smv` repositories from its own location. Every invocation runs the complete pipeline: clean Intel builds, Verification/Validation, CEditQt checks, figures, and manuals. It uses Python, LaTeX, and the scheduler selected by `-q`. Standalone suite execution does not require the Smokeview or manual toolchains.

## Shared repository preparation

Keep one set of repositories under a common directory, such as `/home/firebot/firemodels`. The local `update_repos.sh` in that directory updates and cleans the repositories before any bot starts. It is maintained outside version control. CFASTbot uses the prepared revisions and performs no Git updates or cleanup.

Run preparation and CFASTbot in sequence, so a preparation failure prevents CI:

```bash
cd /home/firebot/firemodels
./update_repos.sh && ./cfast/Utilities/CI/run_cfastbot.sh -q firebot -U
```

Run these commands manually in order. Complete repository preparation before starting a bot, and let the bot finish before preparing repositories again.

A CFAST-only cron entry after repository preparation is:

```cron
1 3 * * * /bin/bash -lc '/home/firebot/firemodels/cfast/Utilities/CI/run_cfastbot.sh -q firebot -U > /home/firebot/.cfastbot/cfastbot_daily.out 2>&1'
```

Create `/home/firebot/.cfastbot` before using that log redirection. CFASTbot needs no workspace initialization or repository configuration file.

## State and Python

State defaults to `$HOME/.cfastbot`; override it with `CFAST_CI_STATE_DIR`. It must be outside the cleaned checkouts. It contains per-run logs under `runs/`, history, and the Python environment. `latest_run` records the most recent log directory. Each run records repository and submodule revisions and its final exit code. Successful runs save PDFs and revision metadata in `VERSION`; `VERSION_LATEST` contains metadata for the current run.

The managed Python environment is installed from `fds/.github/requirements.txt` and reused until those requirements change. `CFAST_CI_PYTHON` may select an already-provisioned interpreter instead. FDS's plotting module is imported from the selected FDS checkout. Linux bundle builds also require PyInstaller in this environment.

## Bundles and publishing

CFAST platform bundles are built with the scripts in [`Build/bundle`](../../Build/bundle/README.md). On Linux, `-U` builds CEditQt and calls `Build/bundle/build_linux_bundle.sh` directly to package the tested CFAST and Smokeview executables and the generated manuals. Repository updates and CFAST, Smokeview, and manual rebuilds are disabled during this packaging step. The tarball and staging files are stored in the run's log directory.

`-U` uploads the manuals, revision information, and Linux tarball to the GitHub release selected by `GH_OWNER`, `GH_REPO`, and `GH_CFAST_TAG`. GitHub CLI (`gh`) must be installed and authenticated. The macOS and Windows platform scripts can use these manuals with `--manuals-from-release`.
