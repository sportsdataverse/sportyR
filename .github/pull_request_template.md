<!--
Title this PR with a Conventional Commit, e.g. `fix(football): place NCAA hashmarks at 60 ft`.
PRs are squash-merged, so the title decides the release: `fix:` = patch, `feat:` = minor,
`!` = major, and `docs:`, `test:`, `ci:`, `chore:`, etc. don't release.
See CONTRIBUTING.md for details.
-->

## What does this PR do?



## Related issue

Closes #

## Sources

<!-- For new surfaces or dimension changes: the rule book edition, rule number, and a link. Delete if not applicable. -->



## Checklist

- [ ] The PR title follows Conventional Commits
- [ ] The PR addresses one issue or change
- [ ] `devtools::document()` has been run and the `man/` changes are committed
- [ ] `devtools::check()` passes locally
- [ ] `lintr::lint_package()` reports no new lints
- [ ] New or changed code is covered by tests in `tests/testthat/`
- [ ] `DESCRIPTION`'s `Version` and `NEWS.md`'s version headings are unchanged (notes, if any, go under `# sportyR (development version)`)
- [ ] If this adds a sport, I've listed myself under General Managers in `README.Rmd` and `README.md`
