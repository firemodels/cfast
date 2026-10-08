"""Regression tests for CI launchers; all mutable fixtures live in temporary directories.

Run: python Utilities/CI/test_ci.py
Set CFAST_CI_INTEGRATION=1 to also run a real CFAST case and CEditQt rewrite.
"""
from __future__ import annotations
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

CI = Path(__file__).resolve().parent
CFAST = CI.parents[1]
BOT = Path(os.environ.get("CFAST_CI_BOT_REPO", CFAST.parent / "bot"))
LAUNCHER = CFAST / "Utilities/scripts/qcfast.sh"


def run(args, *, env=None, cwd=None, ok=True):
    result = subprocess.run([str(x) for x in args], env=env, cwd=cwd, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=120)
    if ok and result.returncode:
        raise AssertionError(f"{args}: exit {result.returncode}\n{result.stdout}")
    return result


def executable(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)
    path.chmod(0o755)
    return path


class CaseLaunchTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="cfast ci ")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.env = dict(os.environ, PATH=f"{self.bin}:{os.environ['PATH']}",
                        CFAST_CI_STATUS_GRACE="0", CFAST_CI_POLL_SECONDS="0.01")
        (self.root / "fire.in").write_text("input\n")
        self.exe = executable(self.bin / "model", '#!/bin/bash\nprintf "%s\\n" "$@"\nexit "${MODEL_EXIT:-0}"\n')
        self.manifest = self.root / "jobs"
        self.env["CFAST_JOB_MANIFEST"] = str(self.manifest)

    def launch(self, *args, ok=True):
        return run([LAUNCHER, "-d", self.root, "-e", self.exe, *args, "fire.in"], env=self.env, ok=ok)

    def wait(self, ok=True):
        return run(["bash", "-c", 'source "$1"; wait_case_manifest "$2"', "test",
                    CI / "wait_cases.sh", self.manifest], env=self.env, ok=ok)

    def test_preview_preserves_outputs(self):
        status = self.root / "fire.exit"
        status.write_text("previous")
        result = self.launch("-q", "terminal", "-v")
        self.assertIn("fire.in", result.stdout)
        self.assertEqual(status.read_text(), "previous")
        self.assertFalse(self.manifest.exists())

    def test_local_success_and_failure(self):
        self.launch("-q", "terminal")
        self.wait()
        self.env["MODEL_EXIT"] = "17"
        self.assertEqual(self.launch("-q", "terminal", ok=False).returncode, 17)
        self.assertEqual((self.root / "fire.exit").read_text().strip(), "17")
        self.assertNotEqual(self.wait(ok=False).returncode, 0)

    def test_background_completion(self):
        self.launch("-q", "none")
        self.wait()
        self.assertEqual((self.root / "fire.exit").read_text().strip(), "0")

    def test_stop_and_iteration_controls(self):
        self.launch("-s")
        self.assertTrue((self.root / "fire.stop").exists())
        self.env["STOPFDSMAXITER"] = "2"
        self.launch("-q", "terminal", "-c", "-V")
        self.assertEqual((self.root / "fire.stop").read_text().strip(), "2")
        self.assertIn("fire\n-V\n", (self.root / "fire.log").read_text())
        del self.env["STOPFDSMAXITER"]
        self.launch("-q", "terminal")
        self.assertFalse((self.root / "fire.stop").exists())

    def test_slurm_pbs_and_submission_failure(self):
        sbatch = executable(self.bin / "sbatch", '#!/bin/bash\nbash "$1" >/dev/null 2>&1\necho "Submitted batch job 123"\n')
        self.assertIn("Submitted batch job 123", self.launch("-q", "batch").stdout)
        self.wait()
        self.assertIn("#SBATCH --cpus-per-task=1", (self.root / "fire.slog").read_text())
        executable(sbatch, '#!/bin/bash\nexit 19\n')
        self.assertNotEqual(self.launch("-q", "batch", ok=False).returncode, 0)
        sbatch.unlink()
        executable(self.bin / "qsub", '#!/bin/bash\nbash "${!#}" >/dev/null 2>&1\necho "456.server"\n')
        self.launch("-q", "batch")
        self.wait()
        self.assertIn("#PBS -l nodes=1:ppn=1", (self.root / "fire.slog").read_text())

    def test_disappeared_job_fails(self):
        executable(self.bin / "sbatch", '#!/bin/bash\necho "Submitted batch job 123"\n')
        executable(self.bin / "squeue", '#!/bin/bash\nexit 0\n')
        self.launch("-q", "batch")
        result = self.wait(ok=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("without a status file", result.stdout)

    def test_suite_selection_and_failure_propagation(self):
        for suite in ["Verification", "Validation"]:
            folder = self.root / suite / "scripts"
            folder.mkdir(parents=True)
            (folder.parent / "one.in").write_text("input")
            (folder.parent / "two.in").write_text("input")
            (folder / "CFAST_Cases.sh").write_text('$RUNCFAST -d . one.in\n$RUNCFAST -d . two.in\n')
            run([CI / "Run_CFAST_Cases.sh", "--repo-root", self.root, "--suite", suite,
                 "-q", "terminal", "-e", self.exe, "--case", "one"], env=self.env)
            self.assertTrue((folder.parent / "one.exit").exists())
            self.assertFalse((folder.parent / "two.exit").exists())
        self.env["MODEL_EXIT"] = "9"
        result = run([CI / "Run_CFAST_Cases.sh", "--repo-root", self.root, "--suite", "Verification",
                      "-q", "terminal", "-e", self.exe], env=self.env, ok=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((self.root / "Verification/two.exit").exists())

    @unittest.skipUnless(os.environ.get("CFAST_CI_INTEGRATION"), "Set CFAST_CI_INTEGRATION=1")
    def test_real_model_and_ui(self):
        shutil.copy2(CFAST / "Verification/Sprinkler/e_coefficient.in", self.root / "fire.in")
        platform = "macos" if sys.platform == "darwin" else "linux"
        self.exe = CFAST / f"Build/CFAST/gnu_{platform}_db/cfast8_{platform}_db"
        self.launch("-q", "terminal")
        self.env.update(CFAST_REPO=str(CFAST), CFAST_PYTHON=sys.executable,
                        MPLCONFIGDIR=str(self.root / "matplotlib"))
        self.launch("-q", "terminal", "--test-UI")
        self.assertEqual((self.root / "fire.ui.exit").read_text().strip(), "0")
        self.assertIn("1/1 passed", (self.root / "fire.ui.log").read_text())


@unittest.skipUnless((BOT / "Scripts/run_cfast_ci.sh").exists(), "Requires sibling bot checkout")
class PreparationTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="cfast-preparation-")
        self.addCleanup(self.temp.cleanup)
        self.base = Path(self.temp.name).resolve()
        self.root = self.base / "repos"
        self.root.mkdir()
        self.env = dict(os.environ, CFAST_CI_STATE_DIR=str(self.base / "state"))
        self.launcher = BOT / "Scripts/run_cfast_ci.sh"
        self.sha = run(["git", "-C", BOT, "rev-parse", "HEAD"]).stdout.strip()
        # Existing commits only: tests never commit, merge, push, or touch source checkouts.
        for name in ["cfast", "fds", "exp", "smv"]:
            run(["git", "clone", "--shared", "--no-checkout", BOT, self.root / name])
            run(["git", "-C", self.root / name, "checkout", "--detach", self.sha])
            self.env[f"CFAST_CI_{name.upper()}_REF"] = self.sha

    def prepare(self, *args, ok=True):
        return run([self.launcher, "--root", self.root, *args], env=self.env, ok=ok)

    def test_unmarked_checkout_is_not_cleaned(self):
        dirty = self.root / "cfast/keep-me"
        dirty.write_text("local work")
        self.assertNotEqual(self.prepare("-C", "--prepare-only", ok=False).returncode, 0)
        self.assertTrue(dirty.exists())

    def test_update_reuses_git_and_cleans_outputs(self):
        self.prepare("--init-workspace")
        gitdir = self.root / "cfast/.git"
        inode = gitdir.stat().st_ino
        tracked = self.root / "cfast/README.md"
        tracked.write_text("modified tracked file")
        output = self.root / "cfast/extra-output"
        output.write_text("generated")
        ignored = self.root / "cfast/ignored-cache"
        ignored.mkdir()
        (ignored / "data").write_text("cache")
        with (gitdir / "info/exclude").open("a") as stream:
            stream.write("\nignored-cache/\n")
        self.prepare("-C", "--prepare-only")
        self.assertEqual(gitdir.stat().st_ino, inode)
        self.assertFalse(output.exists())
        self.assertFalse(ignored.exists())
        self.assertNotEqual(tracked.read_text(), "modified tracked file")
        manifest = self.base / "state/prepared_revisions.tsv"
        self.assertEqual(len(manifest.read_text().splitlines()), 4)
        self.prepare("-C", "--prepare-only")
        self.assertEqual(gitdir.stat().st_ino, inode)

    def test_invalid_ref_fails_before_reset(self):
        self.prepare("--init-workspace")
        tracked = self.root / "cfast/README.md"
        tracked.write_text("preserve on preflight failure")
        self.env["CFAST_CI_SMV_REF"] = "does-not-exist"
        self.assertNotEqual(self.prepare("-C", "--prepare-only", ok=False).returncode, 0)
        self.assertEqual(tracked.read_text(), "preserve on preflight failure")

    def test_release_revision_and_hash_aliases(self):
        self.prepare("--init-workspace")
        config = self.base / "config.sh"
        config.write_text(f'export BUNDLE_CFAST_REVISION={self.sha}\nexport BUNDLE_EXP_HASH={self.sha}\nexport BUNDLE_SMV_HASH={self.sha}\n')
        self.prepare("-F", config, "--prepare-only")
        self.assertIn(self.sha, (self.base / "state/prepared_revisions.tsv").read_text())

    def test_release_keeps_tooling_when_pinned_source_predates_ci(self):
        self.prepare("--init-workspace")
        tooling = self.root / "cfast/Utilities/CI"
        tooling.mkdir(parents=True)
        executable(tooling / "run_cfastbot.sh", '#!/bin/bash\nprintf "%s\\n" "$@" > "$TOOLING_CAPTURE"\n')
        executable(self.root / "cfast/Utilities/scripts/qcfast.sh", '#!/bin/bash\nexit 0\n')
        config = self.base / "config.sh"
        config.write_text(f'export BUNDLE_CFAST_REVISION={self.sha}\nexport BUNDLE_EXP_HASH={self.sha}\nexport BUNDLE_SMV_HASH={self.sha}\n')
        self.env["TOOLING_CAPTURE"] = str(self.base / "tooling-args")
        self.prepare("-F", config, "--preflight")
        self.assertFalse(tooling.exists())
        args = (self.base / "tooling-args").read_text().splitlines()
        self.assertEqual(Path(args[args.index("--root") + 1]).resolve(), self.root.resolve())
        self.assertIn("--preflight", args)

    def test_active_lock_cannot_be_forced(self):
        lock = self.base / "state/run.lock"
        lock.mkdir(parents=True)
        (lock / "pid").write_text(str(os.getpid()))
        self.assertNotEqual(self.prepare("-f", "--prepare-only", ok=False).returncode, 0)
        self.assertTrue((lock / "pid").exists())


class BundleTests(unittest.TestCase):
    def test_linux_archive_and_upload(self):
        with tempfile.TemporaryDirectory(prefix="cfast bundle ") as tmp:
            root = Path(tmp).resolve()
            repo = root / "cfast"
            script = repo / "Build/bundle/build_linux_bundle.sh"
            script.parent.mkdir(parents=True)
            shutil.copy2(CFAST / "Build/bundle/build_linux_bundle.sh", script)
            exe = executable(root / "model", '#!/bin/bash\nexit 0\n')
            data = repo / "Utilities/for_bundle/Bin/Data"
            data.mkdir(parents=True)
            (data / "Users_Guide_Example.in").write_text("fixture")
            (data / "Large_Building.in").write_text("fixture")
            for guide in ["Tech_Ref", "Users_Guide", "Validation_Guide", "Configuration_Guide"]:
                pdf = repo / f"Manuals/CFAST_{guide}/CFAST_{guide}.pdf"
                pdf.parent.mkdir(parents=True)
                pdf.write_text("fixture PDF")
            fakebin = root / "bin"
            executable(fakebin / "uname", '#!/bin/bash\necho Linux\n')
            executable(fakebin / "ldd", '#!/bin/bash\nexit 0\n')
            executable(fakebin / "gh", '#!/bin/bash\nprintf "%s\\n" "$@" > "$UPLOAD_CAPTURE"\nexit "${UPLOAD_EXIT:-0}"\n')
            env = dict(os.environ, PATH=f"{fakebin}:{os.environ['PATH']}",
                       UPLOAD_CAPTURE=str(root / "uploaded"))
            args = [script, "--name", "CFAST-test-linux", "--no-update-repos", "--no-build-cfast",
                    "--no-build-smokeview", "--no-build-manuals", "--no-upload-manuals",
                    "--no-cedit", "--no-smokeview", "--cfast-exe", exe,
                    "--upload-release-repo", "fixture/bundles", "--upload-release-tag", "TEST"]
            run(args, env=env)
            self.assertFalse((root / "uploaded").exists())
            run([*args, "--upload"], env=env)
            archive = repo / "Build/bundle/linux/CFAST-test-linux.tar.gz"
            self.assertTrue(archive.is_file())
            contents = run(["tar", "-tzf", archive]).stdout
            self.assertIn("CFAST/CFAST8/bin/cfast8_linux", contents)
            self.assertIn("Documentation/CFAST_Users_Guide.pdf", contents)
            uploaded = (root / "uploaded").read_text().splitlines()
            self.assertEqual(uploaded, ["release", "upload", "TEST", str(archive), "--clobber", "-R", "fixture/bundles"])
            env["UPLOAD_EXIT"] = "17"
            self.assertNotEqual(run([*args, "--upload"], env=env, ok=False).returncode, 0)
            self.assertIn("requires tarball", run([*args, "--upload", "--no-tarball"], env=env, ok=False).stdout)

    def test_shell_syntax(self):
        paths = list(CI.glob("*.sh")) + [LAUNCHER, BOT / "Scripts/run_cfast_ci.sh",
            CFAST / "Build/bundle/build_linux_bundle.sh"]
        for path in paths:
            with self.subTest(path=path):
                run(["bash", "-n", path])


class PipelineTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="cfast-pipeline-")
        self.addCleanup(self.temp.cleanup)
        self.base = Path(self.temp.name).resolve()
        self.root = self.base / "repos"
        self.bin = self.base / "bin"
        self.bin.mkdir()
        for repo in ["cfast", "fds", "smv", "exp"]:
            (self.root / repo).mkdir(parents=True)
        self.env = dict(os.environ, PATH=f"{self.bin}:{os.environ['PATH']}",
                        CFAST_CI_STATE_DIR=str(self.base / "state"), CFAST_CI_STATUS_GRACE="0",
                        CFAST_CI_POLL_SECONDS="0.01", CAPTURE=str(self.base / "mail"))
        for name in ["ifx", "pdflatex", "biber"]:
            executable(self.bin / name, '#!/bin/bash\necho "fixture tool"\n')
        executable(self.bin / "git", '#!/bin/bash\ncase " $* " in *" submodule "*) exit 0;; *" describe "*) echo CFAST-test;; *) echo abc123;; esac\n')
        executable(self.bin / "mail", '#!/bin/bash\ncat >> "$CAPTURE"\n')
        py = executable(self.bin / "python", '#!/bin/bash\nif [[ "$1" == "${FAIL_PYTHON_STAGE:-never}" ]]; then exit 11; fi\necho "fixture Python passed"\n')
        self.env["CFAST_CI_PYTHON"] = str(py)
        platform = "linux"
        smv_platform = "linux"
        executable(self.bin / "uname", '#!/bin/bash\necho Linux\n')
        for suffix in ["", "_db"]:
            path = self.root / f"cfast/Build/CFAST/intel_{platform}{suffix}/make_cfast.sh"
            executable(path, f'#!/bin/bash\n[[ "$1" == --clean-cfast ]] || exit 20\ncat > cfast8_{platform}{suffix} <<\'MODEL\'\n#!/bin/bash\necho "fixture model"\nexit "${{MODEL_EXIT:-0}}"\nMODEL\nchmod +x cfast8_{platform}{suffix}\n')
        executable(self.root / f"smv/Build/LIBS/intel_{smv_platform}/make_LIBS.sh", '#!/bin/bash\necho "libraries built"\n')
        for script, suffix in [("make_smokeview.sh", ""), ("make_smokeview_db.sh", "_db")]:
            executable(self.root / f"smv/Build/smokeview/intel_{smv_platform}/{script}",
                       f'#!/bin/bash\ntouch smokeview_{smv_platform}{suffix}\n')
        for suite in ["Verification", "Validation"]:
            folder = self.root / f"cfast/{suite}/scripts"
            folder.mkdir(parents=True)
            (folder.parent / "fire.in").write_text("fixture")
            (folder / "CFAST_Cases.sh").write_text('$RUNCFAST -d . fire.in\n')
        executable(self.root / "cfast/Validation/scripts/Make_CFAST_Pictures.sh", '#!/bin/bash\necho "pictures complete"\n')
        (self.root / "cfast/Utilities/Python").mkdir(parents=True)
        for guide in ["Tech_Ref", "Users_Guide", "Validation_Guide", "Configuration_Guide"]:
            name = f"CFAST_{guide}"
            executable(self.root / f"cfast/Manuals/{name}/make_guide.sh",
                       f'#!/bin/bash\ntouch {name}.pdf\necho "{name} build succeeded"\n')
        stats = self.root / "cfast/Manuals/CFAST_Validation_Guide/SCRIPT_FIGURES/Scatterplots"
        stats.mkdir(parents=True)
        for suffix in ["", "_baseline"]:
            (stats / f"validation_scatterplot_output{suffix}.csv").write_text("same\n")

    def pipeline(self, *args, ok=True):
        return run([CI / "run_cfastbot.sh", "--root", self.root, "-q", "terminal", "-m", "fixture", *args],
                   env=self.env, ok=ok)

    def test_complete_pipeline_and_version_contract(self):
        self.pipeline("--test-UI")
        state = self.base / "state"
        output = Path((state / "latest_run").read_text().strip())
        self.assertEqual((output / "exit_code").read_text().strip(), "0")
        self.assertTrue((output / "status_success").exists())
        self.assertTrue((state / "VERSION/CFAST_HASH").exists())
        self.assertTrue((state / "VERSION/SMV_HASH").exists())
        self.assertTrue((state / "VERSION/CFAST_Users_Guide.pdf").exists())
        self.assertTrue((state / "last_successful_revisions.tsv").exists())
        self.assertFalse((state / "run.lock").exists())
        self.assertIn("unchanged", self.pipeline("-a").stdout)

    def test_model_failure_fails_pipeline(self):
        self.env["MODEL_EXIT"] = "7"
        result = self.pipeline(ok=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((self.base / "state/VERSION").exists())
        self.assertFalse((self.base / "state/run.lock").exists())

    def test_silent_python_failure_is_not_success(self):
        self.env["FAIL_PYTHON_STAGE"] = "CFAST_verification_script.py"
        result = self.pipeline(ok=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((self.base / "state/VERSION").exists())
        output = Path((self.base / "state/latest_run").read_text().strip())
        self.assertIn("exit 11", (output / "errors").read_text())

    def prepare_bundle(self):
        target = self.root / "cfast/Build/bundle/build_linux_bundle.sh"
        executable(target, '#!/bin/bash\nprintf "%s\\n" "$@" > "$BUNDLE_CAPTURE"\nexit "${BUNDLE_EXIT:-0}"\n')
        executable(self.root / "cfast/Build/CeditQt/build_linux_app.sh",
                   '#!/bin/bash\nprintf "%s\\n" "$@" > "$QT_CAPTURE"\nexit "${QT_EXIT:-0}"\n')
        executable(self.bin / "gh", '#!/bin/bash\nprintf "%s\\n" "$@" >> "$GH_CAPTURE"\n')
        self.env.update(BUNDLE_CAPTURE=str(self.base / "bundle-args"),
                        QT_CAPTURE=str(self.base / "qt-args"), GH_CAPTURE=str(self.base / "gh-args"),
                        GH_OWNER="fixture", GH_REPO="bundles", GH_CFAST_TAG="TEST")

    def test_bundle_uses_verified_artifacts(self):
        self.prepare_bundle()
        self.pipeline("-U")
        args = (self.base / "bundle-args").read_text().splitlines()
        for flag in ["--no-update-repos", "--no-build-cfast", "--no-build-smokeview",
                     "--no-build-manuals", "--no-upload-manuals", "--upload"]:
            self.assertIn(flag, args)
        self.assertEqual(args[args.index("--cfast-exe") + 1],
                         str(self.root / "cfast/Build/CFAST/intel_linux/cfast8_linux"))
        self.assertEqual(args[args.index("--smokeview-exe") + 1],
                         str(self.root / "smv/Build/smokeview/intel_linux/smokeview_linux"))
        self.assertEqual(args[args.index("--upload-release-repo") + 1], "fixture/bundles")
        self.assertEqual(args[args.index("--upload-release-tag") + 1], "TEST")
        self.assertEqual((self.base / "qt-args").read_text().splitlines(),
                         ["--python", self.env["CFAST_CI_PYTHON"]])
        self.assertIn("CFAST_INFO.txt", (self.base / "gh-args").read_text())

    def test_bundle_failure_fails_pipeline(self):
        self.prepare_bundle()
        self.env["BUNDLE_EXIT"] = "19"
        self.assertNotEqual(self.pipeline("-U", ok=False).returncode, 0)
        self.assertFalse((self.base / "state/VERSION").exists())
        self.assertFalse((self.base / "gh-args").exists())

    def test_cedit_failure_prevents_packaging(self):
        self.prepare_bundle()
        self.env["QT_EXIT"] = "20"
        self.assertNotEqual(self.pipeline("-U", ok=False).returncode, 0)
        self.assertFalse((self.base / "bundle-args").exists())
        self.assertFalse((self.base / "gh-args").exists())


if __name__ == "__main__":
    unittest.main(verbosity=2)
