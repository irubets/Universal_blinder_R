# Clean-library installation verification: 3.1.0

**Result: PASS.** Verified November 13, 2026, on Windows with R 4.6.1.

## Published asset tested

Downloaded [universalblinder_3.1.0.tar.gz](https://github.com/irubets/Universal_blinder_R/releases/download/v3.1.0/universalblinder_3.1.0.tar.gz) from the public [3.1.0 release](https://github.com/irubets/Universal_blinder_R/releases/tag/v3.1.0), rather than reusing the local build. This is the README-updated package built from commit `3240f4d370cfc7c6a35985d961d825a125937f0d`.

- Size: 7,678,671 bytes.
- SHA-256: `21f96fd69d32d4e03e5e9775691f8657918dcc89f046aef562e6ae0f05f04016`.
- The downloaded checksum matches the published release asset.

## Installation

- Started with an empty, isolated R library.
- Installed dependencies afresh from CRAN, then installed the downloaded source archive using `install.packages(..., repos = NULL, type = "source")`.
- The R library search path contained only the new library and the base R system library. The existing user library was excluded.
- Verified installed version `3.1.0` and confirmed that the loaded package came from the new library.
- Recorded the [73 installed package versions](verification-3.1.0/installed-packages.csv), including universalblinder. Local library paths are omitted from this public record.

## Application and installed documentation

- Launched `universalblinder::run_app()` from the fresh installation.
- Headless Chrome connected to the Shiny websocket. **Setup / About** reported version 3.1.0 and no missing dependencies.
- The installed README matches the latest repository README and contains the release, R package and HTML-guide links plus the updated installation instructions.
- The installed in-app `USERGUIDE.md`, 15 PNG screenshots and workflow SVG are present. The standalone `USERGUIDE.html` is a separate release download; it is not installed by the R package.
- Opened the user guide inside the running app and decoded all 16 images: 15 screenshots plus the workflow diagram.
- README and user-guide resources returned HTTP 200. No JavaScript errors occurred.
- The test app was stopped after verification. No original study datasets were loaded and no AI requests were made.

[Structured browser verification results](verification-3.1.0/browser-verification.json) are included. Detailed installation logs and screenshots were retained locally and are not part of this public report.

## Observed warning

The current CRAN Shiny binary emitted a warning that it was built under **R 4.6.2**, while this test used **R 4.6.1**. Package loading, application launch and documentation checks still passed. This verification establishes the tested Windows/R combination; it does not constitute installation testing on every operating system or every supported R version.
