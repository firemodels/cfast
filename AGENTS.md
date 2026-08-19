# CFAST Repository Guidance

These rules adapt the firemodels [Developer Commit Guidelines](https://github.com/firemodels/fds/wiki/Developer-Commit-Guidelines) for CFAST. Follow more specific instructions in nested `AGENTS.md` files when present.

## Scope and file safety

- Treat the Git repository root as the working root.
- Edit only files already tracked by Git unless the user explicitly asks to add a new file.
- Check `git status` before editing. Preserve unrelated user changes and do not overwrite replacement copies outside the repository.
- Never delete generated figures or use destructive synchronization options unless the user explicitly requests deletion and the exact source and destination have been verified.

## Changes, tests, and commits

- Keep changes small, focused, and organized. Commit early and often when commits are requested, and avoid mixing unrelated work.
- Add or update a verification test for each code change whenever practical. A code change without a test has no guarantee of surviving later development.
- Add new verification and validation cases to the applicable guides.
- Update the theory/technical reference, validation guide, user guide, configuration guide, and release notes as applicable. Mark beta features clearly; documented features are generally expected to be available in the next release.
- Use the repository makefiles. Before handing off new source code, build a debug target and require no compiler errors or warnings. Test with GNU and Intel compilers when they are available.
- Before committing, synchronize with the central branch and rerun the relevant debug build and tests. **Pull, merge, rebase, push, and commit operations are human-only; agents must not perform them.**
- Pull requests should contain no merge commits. Keep them to a few coherent commits when possible; coordinate in advance when a large series is unavoidable.
- Do not commit generated binaries or other unintended build artifacts. Treat release PDFs as release-only artifacts.
- Avoid major code or documentation changes near a release unless the maintainers approve them.
- When asked to write a commit message, use a consistent leading tag such as `CFAST Source`, `CFAST User Guide`, `CFAST Tech Guide`, `CFAST Validation Guide`, `CFAST Configuration Guide`, `CFAST Validation`, `CFAST Verification`, `Bibliography`, or `Python`.

## Fortran source style

- Use `IMPLICIT NONE`.
- Write Fortran source in uppercase for readability and searchability.
- Indent blocks by three spaces and never use tabs. Remove trailing and excessive whitespace.
- Use one blank line within a subroutine and two blank lines between subroutines. Leave one blank line below major block comments.
- Group variables of the same type into a single declaration when the result remains readable.
- Name any `IF`, `DO`, or similar construct longer than one screen, and explicitly name the target of `CYCLE` or `EXIT`, for example `CYCLE LOOP_NAME`.
- Prefer descriptive named variables and named `PARAMETER` constants over abbreviations and unexplained numeric literals. Add a concise explanation for hardwired constants.
- Prefer integer values over strings in `SELECT CASE` and similar comparisons.
- Use `>`, `<`, `>=`, `<=`, `==`, and `/=` rather than `.GT.`, `.LT.`, `.GE.`, `.LE.`, `.EQ.`, and `.NE.`. Put spaces around `.AND.` and `.OR.`, but write `.NOT.` directly before its logical variable.
- Do not compare real values directly with `==` or `/=`. Use a tolerance such as `ABS(X-Y) < TWO_EPSILON_EB`, with the logically correct comparison direction for the condition.
- Explicitly specify real precision, for example `0._EB` or `0._FB`, rather than `0.`.
- Add new namelist inputs alphabetically in the source and the User Guide.
- Limit source lines to 132 characters.
- Write `IF (condition) THEN`, with spaces around the parentheses. Use the single words `ENDIF` and `ENDDO`.
- Use `I0` to format integers when alignment is not required.
- Define output units with forms such as `m2`, not `m^2`, so spatial statistics continue to work.
- Write for the next developer who must debug the code. Rewrite code until its intent is clear at a glance.

## Manuals

- Manual sources live under `Manuals/`. Edit the tracked LaTeX sources in place; do not create alternate or replacement appendix files.
- Build a guide from its manual directory with its existing `make_guide.sh` script.
- After changing a manual, require a successful build, check the LaTeX error log, and visually inspect representative affected PDF pages.
- Follow the mathematical and SI-unit style of Thompson and Taylor, *Guide for the Use of the International System of Units*, NIST Special Publication 811.
- Add every new input parameter to the User Guide. If it is not ready for public display, include the documentation as a LaTeX comment so developers can track it.
- Do not add or update release PDF collections except as part of an explicitly requested release.
- Avoid web links in manuals because they frequently become stale.
- Use soft wrapping for prose: do not insert hard line breaks within ordinary paragraphs. Reformat hard-wrapped paragraphs when already editing that section.
- Put shared custom commands and LaTeX variables in `Manuals/Bibliography/commoncommands.tex` rather than redefining them locally.
- Use modern LaTeX commands such as `\textbf{}` and `\texttt{}` rather than deprecated declarations such as `\bf` and `\tt`.
- Give every section, figure, and table caption a concise optional form for the Table of Contents or lists, without a final period, for example `\caption[Short caption]{Full caption.}`. Keep list entries to one line with at least one leader dot before the page number.
- Follow existing plot sizing and centering conventions. Do not resize plots arbitrarily; use the established dimensions unless the content requires an exception.
- For CFAST User Guide namelist documentation, follow the FDS User Guide style: list namelist sections alphabetically, list parameters alphabetically within each table, and include useful section labels and references.
- Keep LaTeX table source compact and readable in a monospace editor. Align the `&` column separators to reasonable fixed stops. Do not pad ordinary rows to the length of long footnotes or other exceptional cells; place a long first-column footnote on its own source line when needed.
- Use callout styling such as `\graybox` only when the content genuinely needs emphasis. Prefer ordinary prose or a short list for routine explanatory text.
- Keep required-input markers and other table footnotes unambiguous. Do not combine redundant lettered and asterisk markers.

## Verification

- Run `git diff --check` before handing off changes.
- Compile affected verification and validation guides before changing their cases, and recompile them after the change.
- Avoid hyphens in new verification and validation filenames because they interfere with command-line completion on some systems.
- Report exactly which tracked source files changed and whether the relevant build succeeded.
