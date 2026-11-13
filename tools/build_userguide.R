# Build USERGUIDE.html from USERGUIDE.md (one self-contained file: the workflow diagram and the style are embedded).
# Run from the package folder after every change to USERGUIDE.md:
#   Rscript tools/build_userguide.R        (or source("tools/build_userguide.R") in RStudio)
# Needs pandoc: the copy that comes with RStudio is found too.

pandoc <- Sys.which("pandoc")
if (!nzchar(pandoc) && requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available())
  pandoc <- file.path(rmarkdown::find_pandoc()$dir, "pandoc")
if (!nzchar(pandoc)) {
  cand <- c(file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc", "pandoc.exe"), "C:/Program Files/Pandoc/pandoc.exe",
            Sys.glob("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/*/pandoc.exe"),
            Sys.glob("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/pandoc.exe"))
  pandoc <- cand[file.exists(cand)][1]
}
if (is.na(pandoc) || !nzchar(pandoc)) stop("pandoc not found: install it (https://pandoc.org) or use the one in RStudio.")

if (!file.exists("USERGUIDE.md")) stop("Run this from the package folder (the one with USERGUIDE.md).")
args <- c("USERGUIDE.md", "-f", "gfm", "-t", "html5", "--standalone", "--embed-resources",
          "--css", "tools/userguide.css", "--include-after-body", "tools/userguide-viewer.html", "--metadata", "pagetitle=Universal Blinder: user guide", "--metadata", "lang=en",
          "-o", "USERGUIDE.html")
st <- system2(pandoc, shQuote(args))
if (!identical(as.integer(st), 0L)) stop("pandoc failed (exit ", st, ")")
message("Written: ", normalizePath("USERGUIDE.html"))
