#!/bin/bash
set -eu
exec python3 -m http.server 8767 --bind 127.0.0.1 --directory "$HOME/.cfastbot/online-docs"
