---
name: report-package-updates
description: Inspect and summarize the content-level changes represented by updated package.el VC revisions, then commit those init.el pins with the summaries. Use after a package update has changed :vc revisions in init.el.
compatibility: Requires Git and local package-vc checkouts under elpa/.
---

Summarize the changed `:vc` revisions in `init.el`, then commit those pin changes with the report as the commit message.

## Local settings

- Emacs configuration directory: `~/.emacs.d`
- Version pins to review and commit: `:vc` revisions in `init.el`
- Old version: those revisions as stored at `HEAD`
- New version: the working-tree `init.el` pins, written after the user's completed package update
- Package history: the Git checkout at `elpa/<package>`

Edit these settings when the local layout changes.

## Workflow

1. Start in the Emacs configuration directory. Inspect `git status --short` and compare the `:vc` revisions in `HEAD:init.el` with the working tree. The old revisions are the pins from the previous commit. The new revisions are the pins written after the user's package update.
2. Identify every package whose revision changed, including its old and new revision. The package name is the symbol in `:vc (PACKAGE ...)`. When `:vc` has no package symbol, the package name is the `use-package` name. A checkout that moved without a pin change is outside this report. If no revision changed, report that and stop.
3. For each changed package, inspect `elpa/<package>` across the old-to-new revision range. A revision may be a commit or a tag.

   - Read the commit subjects with `git log --oneline <old>..<new>`.
   - Inspect the changed-file list and diff statistics with `git diff --name-status <old>..<new>` and `git diff --stat <old>..<new>`.
   - Read the substantive file diffs. Give particular attention to user-visible code, documentation, and changed release-note files such as `NEWS`, `CHANGELOG`, or `CHANGES`.
   - Treat newly added historical release notes as context. Verify against the old revision whether a documented feature is actually new in the selected range.
   - When commit subjects are generic, derive the summary from the changed content.

4. Produce a short report formatted as a ready-to-copy commit message. Use the fixed, capitalized subject line `Scheduled update package versions` with no final period, followed by a blank line. Then write one package summary per line in `package: Summary.` form. Capitalize each summary and end it with a period. Include only those summaries; use the inspected commit ranges and content as evidence.

   At the end of the commit message, include an attribution line formatted as:
   `Reported and summarized by {{Agent}} ({{Model}}).`
   - Use the agent's display name for `{{Agent}}` (e.g. `Claude`, `Codex`, `Antigravity`, `Grok Build`).
   - Use the full, canonical model name for `{{Model}}` (e.g. `Claude Sonnet 5.5`, `GPT-6`, `Gemini 3.8 Flash`, `Grok 4.7`).
   - Leave out thought or reasoning level qualifiers such as `High` or `Low`.

   ```text
   Scheduled update package versions

   eyebrowse: Do X.
   vundo: Do Y.
   zenburn-theme: Do Z.

   Reported and summarized by {{Agent}} ({{Model}}).
   ```
5. If a local package checkout or either revision is unavailable, use a brief line such as `package: Update could not be summarized from local history.`
6. Stage `init.el` only when its diff changes `:rev` values and nothing else. If `init.el` has any other change, or another path is already staged, stop and leave the index unchanged. Verify that the staged diff contains only those revision pins, then create one Git commit using the generated report as its complete commit message.
7. Report the new commit hash and the final `git status --short` output.

## Safety boundaries

- Do not invoke Emacs or run `package-upgrade`, `package-vc-upgrade`, or an archive refresh.
- Stage and commit only the `:vc` revision pins in `init.el`. If another path is already staged, stop and leave the index unchanged.
- Do not use `git add -A`, `git add .`, amend, push, reset, checkout, clean, or any command that modifies unrelated work.
- Do not create an empty commit when no revision changed from `HEAD`.
- If a package revision cannot be mapped to a local checkout or commit range, state that limitation briefly. Do not guess at a summary or attempt to fetch missing history.
