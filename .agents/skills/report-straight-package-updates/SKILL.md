---
name: report-straight-package-updates
description: Report the commit-level changes represented by updated straight.el package revisions and commit the lockfile with those summaries. Use after a straight package update has changed the lockfile.
compatibility: Requires Git and local straight repository checkouts under straight/repos.
---

Summarize the changed revisions in `straight/versions/default.el`, then commit that lockfile with the report as the commit message.

## Local settings

- Emacs configuration directory: `~/.emacs.d`
- Lockfile to review and commit: `straight/versions/default.el`
- Old version: `straight/versions/default.el` as stored at `HEAD`, the previous update commit.
- New version: the current working-tree `straight/versions/default.el`, produced by the user's completed `straight-pull-all` and `straight-freeze-version` run.

Edit these settings when the local layout changes.

## Workflow

1. Start in the Emacs configuration directory. Inspect `git status --short` and compare `HEAD:straight/versions/default.el` with the working-tree lockfile. The old version is the lockfile from the previous update commit; the new version is the lockfile written after the user's package update.
2. Identify every package whose revision changed, including its old and new revision. If there is no lockfile change, report that and stop.
3. For each changed package, inspect the corresponding repository under `straight/repos/<package>` and compare its revisions with `git log --oneline <old>..<new>`.
4. Produce a short report formatted as a ready-to-copy commit message. Use the fixed, capitalized subject line `Scheduled update package versions` with no final period, followed by a blank line. Then write one package summary per line in `package: Summary.` form. Capitalize each summary and end it with a period. Include only those summaries; use the commit ranges only as evidence.

   ```text
   Scheduled update package versions

   eyebrowse: Do X.
   vundo: Do Y.
   zenburn-emacs: Do Z.
   ```
5. If a local package checkout or either revision is unavailable, use a brief line such as `package: Update could not be summarized from local history.`
6. Stage `straight/versions/default.el` by its explicit path. Verify that the staged changes contain exactly that lockfile, then create one Git commit using the generated report as its complete commit message.
7. Report the new commit hash and the final `git status --short` output.

## Safety boundaries

- Do not invoke Emacs or run `straight-pull-all` or `straight-freeze-version`.
- Stage and commit only `straight/versions/default.el`. If another path is already staged, stop and leave the index unchanged.
- Do not use `git add -A`, `git add .`, amend, push, reset, checkout, clean, or any command that modifies unrelated work.
- Do not create an empty commit when the lockfile has no change from `HEAD`.
- If a package revision cannot be mapped to a local repository or commit range, state that limitation briefly. Do not guess at a summary or attempt to fetch missing history.
