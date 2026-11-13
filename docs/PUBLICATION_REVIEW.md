# Public distribution review

Reviewed November 13, 2026, for universalblinder 3.1.0.

## Scope and results

- Reviewed package structure, configuration, blinding/retrieval and release gates, lab workflows, AI data handling, documentation and publication contents.
- R CMD build succeeded. R CMD check --no-manual --no-build-vignettes completed with Status: OK on Windows with R 4.6.1.
- Test suite: 1,590 passing assertions, zero failures and zero warnings. Two repository-only documentation tests skip in the installed-package check; the guide-copy equality, heading coverage, embedded workflow diagram and resource links were independently verified against the source tree.
- The HTML guide and its full-size image viewer were checked with headless Chrome at 320, 390, 768 and 1440 pixel widths. All 16 images load; Enter/Space opening, image scrolling, Escape/Close, focus restoration and contents links pass. No external image resources or JavaScript errors were found.
- Screenshots contain bundled synthetic demonstration records only. A real study-folder identifier in the guide prose was removed before publication.
- Publication contents were compared locally with available private-map terms, without loading original input datasets. Matches remaining in fixtures/examples were reviewed three-digit numeric literals; no unresolved findings remain.
- All 281 pre-publication historical Git blobs were checked for credential patterns and private-data paths. Historical text was also compared with the private-map terms. No unresolved findings remain. Only labelled synthetic data and fixture maps are tracked; real study folders, keys, logs and session histories remain excluded.
- AI-provider tests use mocked responses; this review did not send study data to a hosted model.

## Reproduce the main checks

From a separate working directory with the package dependencies installed:

```r
system2(file.path(R.home("bin"), "R"), c("CMD", "build", "--no-manual", "--no-build-vignettes", shQuote("path/to/Universal_blinder_R")))
system2(file.path(R.home("bin"), "R"), c("CMD", "check", "--no-manual", "--no-build-vignettes", "universalblinder_3.1.0.tar.gz"))
```

Rebuild the standalone guide from the repository root with `Rscript tools/build_userguide.R`. Before future publication, scan an export of the intended public files with `ub_publish_check()` and the relevant local private maps; inspect matches rather than publishing private values in review logs.
