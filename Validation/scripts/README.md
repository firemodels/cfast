# Running CFAST Validation cases

The shared suite runner is `Utilities/CI/Run_CFAST_Cases.sh`. From the CFAST root:

```bash
Utilities/CI/Run_CFAST_Cases.sh --suite Validation -q batch
Utilities/CI/Run_CFAST_Cases.sh --suite Validation -I gnu -q terminal
```

Use `--test-UI` to import and rewrite the inputs through CEditQt without running
CFAST. The `Run_CFAST_Cases.sh` in this directory calls the shared runner. See [the CI documentation](../../Utilities/CI/README.md)
for compiler, debug, queue, stop, timing, and case-selection options.
