# Online manuals

On `master`, activate the shared FDS Python environment. Build the four PDF manuals first (with their figures and `.aux` files). HTML conversion also requires TeX Live (`make4ht`, `biber`, `dvisvgm`) and Poppler (`pdftocairo`, `pdftops`).

From `Manuals/Online_Docs`:

```sh
./build.sh
./serve.sh
```

Open http://127.0.0.1:8767. Stop the server with Ctrl-C.

`build.sh` builds all four English manuals in `build/`, then copies the website to `~/.cfastbot/online-docs/` and creates `~/.cfastbot/online-docs.zip`. Set `CFAST_CI_STATE_DIR` to override that parent directory. `preview/` contains the build tools. Keep these tools on `master`.

Linux CI builds the online manuals after the PDFs and, with `-U`, uploads `online-docs.zip` alongside them to `firemodels/test_bundles`, release `CFAST_TEST`. For GitHub Pages review, manually run **Deploy online manuals** on `master` from the Actions tab; it publishes that ZIP. Generated manuals stay out of Git.

GitHub Pages must have **Settings → Pages → Source → GitHub Actions** selected. The review site is https://firemodels.github.io/cfast/, without NIST branding. NIST's separate deployment from `nist-pages` is unchanged.
