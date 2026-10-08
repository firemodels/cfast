# CFAST continuous integration

CFAST owns the build, Verification/Validation, CEditQt, plotting, manual, and
result-processing pipeline in this directory. The single-case launcher is
`../scripts/qcfast.sh`. The case lists are in `Verification/scripts` and
`Validation/scripts`. These scripts run from any working directory.

## Run a suite

From the CFAST repository root:

```bash
Utilities/CI/Run_CFAST_Cases.sh --suite Verification -I gnu -q terminal
Utilities/CI/Run_CFAST_Cases.sh --suite Validation -I intel -q batch
Utilities/CI/Run_CFAST_Cases.sh --suite Verification --test-UI -q batch
```

Use `-d` for debug, `-m 2` to stop after two iterations, `-s` to request stops,
`-t` to report saved timings, `--case e_coefficient.in` to select a case, and
`-v` to print job scripts without submitting them. `-e` selects an explicit
executable. `-q none` runs cases locally in the background. The
`Verification/scripts/Run_CFAST_Cases.sh` and Validation counterpart forward to
this shared runner.

The launcher supports Slurm and PBS/Torque and records a job manifest when
`CFAST_JOB_MANIFEST` is set. Each completed case writes an `.exit` file. UI tests
use separate `.ui.log`, `.ui.err`, `.ui.slog`, and `.ui.exit` files. The pipeline
waits for its own jobs and fails when a job exits unsuccessfully or disappears
without a completion record. UI jobs import and rewrite inputs; they do not
run CFAST again.

## Run the complete pipeline on existing checkouts

The default repository collection is the parent of this CFAST checkout, with
sibling `fds`, `exp`, and `smv` repositories. Use `--root` to select another
collection:

```bash
/path/to/cfast/Utilities/CI/run_cfastbot.sh --root /path/to/repos -q batch --test-UI
```

This mode does not fetch, switch revisions, or clean repositories. It does
perform clean CFAST builds and generates case results, figures, and manuals.
The complete pipeline requires Intel tools for Smokeview, Python, LaTeX, and a
scheduler when using a batch queue. `-I gnu` selects GNU for CFAST. Standalone
suite execution does not require the Smokeview or manual toolchains.

Use `--preflight` to check prerequisites without building or submitting jobs.
`-a` skips processing when repository revisions match the last successful run;
it is intended for prepared CI checkouts, not detecting uncommitted developer
edits. `-f` clears a stale lock but never bypasses a live run. `-k` terminates the
run and cancels its recorded unfinished jobs. `-m` selects the notification
recipient; `cfastbot_email_list.sh` is also supported.

## Prepare a dedicated CI workspace

Checkout preparation lives outside CFAST in `bot/Scripts/run_cfast_ci.sh` so
that the pipeline does not update its own source while running. Initialize a
**dedicated disposable repository collection**, separately from starting a run:

```bash
/path/to/bot/Scripts/run_cfast_ci.sh --root /path/to/ci-repos --init-workspace
/path/to/bot/Scripts/run_cfast_ci.sh --root /path/to/ci-repos -C -q batch --test-UI
```

`-C` fetches and reuses existing Git repositories. It removes tracked edits
and untracked/ignored files in `cfast`, `fds`, `exp`, and `smv`; use it only in the
marked CI workspace. Missing repositories are cloned once. Existing repositories
are detached at the selected commit, and submodules are synchronized to their
recorded commits. No merges, commits, tags, or pushes are performed.

The default target is `firemodels/master` when that remote exists, otherwise
`origin/master`. Override individual targets with `CFAST_CI_CFAST_REF`,
`CFAST_CI_FDS_REF`, `CFAST_CI_EXP_REF`, and `CFAST_CI_SMV_REF`. The clone base URL
can be set with `CFAST_CI_GIT_BASE_URL`. All targets are resolved before existing
checkouts are reset. `--prepare-only` stops after preparation.

## State and Python

State defaults to `$HOME/.cfastbot`; override it with `CFAST_CI_STATE_DIR`.
It must be outside the cleaned checkouts. It contains the run lock, per-run logs
under `runs/`, history, and the Python environment. `latest_run` records the most
recent log directory. Each run records repository and submodule revisions and
its final exit code. Successful runs save PDFs and revision metadata in `VERSION`;
`VERSION_LATEST` contains metadata for the current run.

The managed Python environment is installed from `fds/.github/requirements.txt`
and reused until those requirements change. `CFAST_CI_PYTHON` may select an
already-provisioned interpreter instead. FDS's plotting module is imported from
the selected FDS checkout. Linux bundle builds also require PyInstaller in this
environment.

## Bundles and publishing

CFAST platform bundles are built with the scripts in
[`Build/bundle`](../../Build/bundle/README.md). On Linux, `-U` builds CEditQt and
calls `Build/bundle/build_linux_bundle.sh` directly to package the tested CFAST
and Smokeview executables and the generated manuals. Repository updates and
CFAST, Smokeview, and manual rebuilds are disabled during this packaging step.
The tarball and staging files are stored in the run's log directory.

`-U` uploads the manuals, revision information, and Linux tarball to the GitHub
release selected by `GH_OWNER`, `GH_REPO`, and `GH_CFAST_TAG`. GitHub CLI (`gh`)
must be installed and authenticated. The macOS and Windows platform scripts can
use these manuals with `--manuals-from-release`.

For a pipeline run at specific revisions, pass `-F /path/to/config.sh` to
`bot/Scripts/run_cfast_ci.sh` in a marked CI workspace. The shell configuration
sets `BUNDLE_CFAST_REVISION`, `BUNDLE_EXP_REVISION`, and `BUNDLE_SMV_REVISION`,
and optionally `BUNDLE_FDS_REVISION`. Each also accepts a `BUNDLE_*_HASH` alias.
Without an FDS revision, the configured FDS remote target is used.

## Tests

```bash
python Utilities/CI/test_ci.py
CFAST_CI_INTEGRATION=1 python Utilities/CI/test_ci.py CaseLaunchTests
```

The tests exercise local execution, simulated Slurm/PBS jobs, pipeline failure
propagation, bundle packaging and publishing dispatch, locks, and update/clean
operations in disposable repositories. They never update the source checkouts or contact GitHub. The
optional integration test uses an existing GNU debug CFAST executable and tests
a real case plus a CEditQt rewrite. A live cluster run is still required to
validate site-specific scheduler and compiler configuration.
