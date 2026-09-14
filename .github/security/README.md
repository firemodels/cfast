# CFAST PR virus scanning

`pr-security.yml` checks the PR author and event sender against
`trusted-developers.json`, then runs ClamAV on every PR. Listed developers receive
`CFAST / PR admission` immediately after the merge snapshot is prepared; others
receive it only after a successful scan. Scanning continues after a PR merges.
`CFAST / ClamAV scan` reports the result separately.

The list uses numeric GitHub account IDs, including `preneke` and `rpeacoc`.
Both author and sender must be listed; a trusted person merging or rerunning an
outside PR does not grant trust. Review list changes and synchronize them to
forks. Organization membership can remain private.

## Repository settings: upstream and every receiving developer fork

1. Commit the security files and workflow to the default branch and each branch
   accepting PRs (normally `master`). Enable workflows in the fork's **Actions**
   tab if GitHub displays a disabled-workflows notice.
2. Click the repository's **Settings** tab in the top navigation row. In the
   settings sidebar, under **Code and automation**, expand **Actions** and select
   **General**. This is separate from the top-level **Actions** tab, which shows
   workflow runs. Direct links: [upstream](https://github.com/firemodels/cfast/settings/actions)
   ; other developers should substitute their fork owner. Repository admin access is required.
   Allow GitHub-authored actions (including `actions/checkout`) and apply the
   settings below, clicking **Save** in each section:

   | Setting | Value |
   | --- | --- |
   | Require actions to be pinned to a full-length commit SHA | Checked; the workflow already pins its action |
   | Artifact and log retention | 90 days |
   | Approval for running fork pull request workflows from contributors | Require approval for all external contributors |
   | Workflow permissions | Read repository contents and packages permissions |
   | Allow GitHub Actions to create and approve pull requests | Unchecked |

   The trusted workflow explicitly requests `statuses: write` for identity/status
   jobs; the read-only default does not prevent this. No additional secrets are
   needed. GitHub's external-contributor approval setting controls PR-supplied
   workflows; our developer list separately controls who waits for ClamAV.
3. Open a small test PR so GitHub registers `CFAST / PR admission` for selection
   as a required status. A listed author and sender should receive admission
   before the scan finishes.
4. In **Settings > Branches**, add or edit a classic protection rule for `master`
   (and other PR target branches). Enable **Require a pull request before merging**,
   **Require status checks to pass before merging**, and **Require branches to be
   up to date before merging**. Require exactly **`CFAST / PR admission`**, choosing
   **GitHub Actions** as its source where offered. Enable **Do not allow bypassing
   the above settings**, including administrators. Leave force pushes and deletions
   disabled. For existing rulesets, use equivalent requirements with **Active**
   enforcement and no bypass actors.
5. Retain maintainer review of workflow and trust-list changes. Require at least
   one approval, or require Code Owner review if a maintained `CODEOWNERS` file
   covers these paths. Do not approve PR-supplied workflows until admission passes
   and their changes are reviewed: an outside PR can add unrelated Actions jobs.
6. Do **not** require `CFAST / ClamAV scan` or the whole workflow if developers must
   merge while scans continue. Use the named admission status, not the job named
   `developer`: `pull_request_target` job checks attach to the base revision.
7. Confirm that an outside PR cannot merge while scanning is pending or failed,
   even when a trusted maintainer attempts the merge. Check that a new commit
   requires fresh admission and that a trusted PR's scan completes after merging.
   Test malware detection with EICAR only in an isolated test repository.

## Scan behavior

Jobs use disposable GitHub-hosted runners and trusted policy code. The scanner
updates ClamAV signatures, checks detection with EICAR, and scans all files in
both the submitted commit and merge result. Git fetches and verifies the objects;
regular blobs are written as data without checkout, hooks, or filters. PR code is
never executed. Scan jobs have read-only permissions and no project secrets.

Links, submodules, unresolved LFS pointers, oversized files, incomplete coverage,
scanner errors, and timeouts fail the scan. Limits are 512 MiB per file, 4 GiB
of source data, and 100,000 files per snapshot. Conflicting or stale PRs are not
admitted. A clean scan does not replace code review or isolated testing.

These files add no build jobs and do not scan installation bundles. External
Firebot or local scripts must independently check successful admission for the
exact PR head before executing outside code. Merge queues need separate
`merge_group` support. Repository settings are not installed by copying YAML.

## Local tests

```sh
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s .github/security -p 'test_*.py' -v
```

Tests mock GitHub/ClamAV and parse YAML with Ruby. They make no network requests
and create no EICAR files. Validate real ClamAV and branch protection on GitHub.
