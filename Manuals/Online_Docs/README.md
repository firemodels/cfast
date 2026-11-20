# Online manuals

On `master`, activate the shared FDS Python environment. Build the four PDF manuals first (with their figures and `.aux` files). HTML conversion also requires TeX Live (`make4ht`, `biber`, `dvisvgm`) and Poppler (`pdftocairo`, `pdftops`).

From `Manuals/Online_Docs`:

```sh
./build.sh
./serve.sh
```

Open http://127.0.0.1:8767. Stop the server with Ctrl-C.

`build.sh` builds all four English manuals in `build/`, then copies the website to `~/.cfastbot/online-docs/` (created if needed). `preview/` contains the build tools. Keep these tools on `master`.

For GitHub Pages review, the **Deploy online manuals** workflow on `master` publishes `online-docs.zip` from the `online-docs-preview` release. The ZIP must contain `index.html` at its root, alongside the other files from `~/.cfastbot/online-docs/`. Upload the ZIP through GitHub Releases, then run the workflow manually from the Actions tab. Generated manuals stay out of Git.

GitHub Pages must have **Settings → Pages → Source → GitHub Actions** selected. The review site is https://firemodels.github.io/cfast/, without NIST branding. NIST's separate deployment from `nist-pages` is unchanged.
