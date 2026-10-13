#!/usr/bin/env bash
# Dedicated CI workspace only: discard local changes and stale Git locks.
# Copy this script beside the repositories and run that copy before any builds.
set -euo pipefail
export GIT_TERMINAL_PROMPT=0
root=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
branch=master
remote_override=
dry_run=0
# Repositories to update and clean.
repos=(fds smv exp out cfast fig cad)
selected_repos=()
usage() {
  cat <<'HELP'
Usage: update_repos.sh [--root directory] [--repo name ...] [options]
Update and clean existing firemodels checkouts; missing repositories are skipped.
  --repo name       Select a repository (repeatable)
  --branch name     Remote branch to check out (default: master)
  --remote name     Remote to fetch (default: firemodels, otherwise origin)
  --dry-run         Show targets and cleanup previews without fetching or changing files
  -h, --help        Show help
Defaults: fds smv exp out cfast fig cad
Remove stale Git locks, fetch tags and full history, and replace each local branch
with the remote branch. Discard local commits, edits, and untracked/ignored files.
Submodules use the commits recorded by the parent.
Run only in the dedicated CI workspace, while no other jobs are using it.
HELP
}
while (($#)); do
  case "$1" in
    --root) root=$2; shift 2;;
    --repo) selected_repos+=("$2"); shift 2;;
    --branch) branch=$2; shift 2;;
    --remote) remote_override=$2; shift 2;;
    --dry-run) dry_run=1; shift;;
    -h|--help) usage; exit 0;;
    *) echo "Unknown option: $1" >&2; exit 2;;
  esac
done
root=$(cd "$root" && pwd -P)
[[ $root != / && $root != "$HOME" ]] || { echo 'Use a repository collection directory.' >&2; exit 1; }
if ((${#selected_repos[@]})); then repos=("${selected_repos[@]}"); fi
for repo in "${repos[@]}"; do
  [[ $repo =~ ^[a-zA-Z0-9_][a-zA-Z0-9_.-]*$ ]] || { echo "Invalid repository name: $repo" >&2; exit 1; }
  path="$root/$repo"
  echo "=== $repo: $path ==="
  [[ ! -L $path ]] || { echo "Refusing symlinked checkout: $path" >&2; exit 1; }
  if [[ ! -e $path ]]; then echo "Skipping missing repository: $repo"; continue; fi
  [[ -d $path/.git && $(git -C "$path" rev-parse --show-toplevel) == "$path" ]] || {
    echo "Not a standalone Git checkout: $path" >&2; exit 1;
  }
  remote=$remote_override
  if [[ -z $remote ]]; then
    remote=origin
    if git -C "$path" remote | grep -qx firemodels; then remote=firemodels; fi
  fi
  git -C "$path" remote get-url "$remote" >/dev/null
  target="refs/remotes/$remote/$branch"
  fetch_options=(--prune --tags --force)
  if [[ $(git -C "$path" rev-parse --is-shallow-repository) == true ]]; then
    fetch_options+=(--unshallow)
  fi
  if [[ $dry_run == 1 ]]; then
    echo "Would remove Git locks, fetch ${fetch_options[*]} $remote, replace $branch with $remote/$branch, and clean $repo"
    git -C "$path" clean -nffdx
    git -C "$path" submodule foreach --recursive 'git clean -nffdx'
    continue
  fi
  echo "Removing stale Git locks (including submodules)"
  find "$path/.git" -type f -name '*.lock' -print -delete
  echo "Fetching $remote/$branch and tags"
  git -C "$path" fetch "${fetch_options[@]}" "$remote" "+refs/heads/$branch:$target"
  echo "Replacing $branch with $remote/$branch and cleaning"
  git -C "$path" checkout --force -B "$branch" "$target"
  git -C "$path" clean -ffdx
  echo "Updating and cleaning submodules"
  git -C "$path" submodule foreach --recursive 'git reset --hard && git clean -ffdx'
  git -C "$path" submodule sync --recursive
  git -C "$path" submodule update --init --recursive --force
  git -C "$path" submodule foreach --recursive 'git reset --hard && git clean -ffdx'
  echo "Finished $repo"
done
if [[ $dry_run == 0 ]]; then echo 'Repository preparation complete.'; fi
