# Online manuals

On `master`, activate the shared FDS Python environment. Build the four PDF manuals first (with their figures and `.aux` files). HTML conversion also requires TeX Live (`make4ht`, `biber`, `dvisvgm`) and Poppler (`pdftocairo`, `pdftops`).

From `Manuals/Online_Docs`:

```sh
./build.sh
./serve.sh
```

Open http://127.0.0.1:8767. Stop the server with Ctrl-C.

`build.sh` builds all four English manuals in `build/`, then copies the website to `~/.cfastbot/online-docs/` (created if needed). `preview/` contains the build tools. Keep these tools on `master`; the outside copy remains available when you switch to `nist-pages` to publish it.
