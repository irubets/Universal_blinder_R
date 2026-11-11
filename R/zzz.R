# Which installed copy this R session loaded, so run_app() can tell when the package was reinstalled since:
# R keeps the code it loaded until the session restarts, so the app would otherwise run the old code silently.
.ub_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .ub_env$pkg_path <- file.path(libname, pkgname)
  .ub_env$install_stamp <- .ub_install_stamp(.ub_env$pkg_path)
}

# modification time of the installed package's Meta/package.rds (rewritten by every install); NA when there is
# none (e.g. devtools::load_all on the source folder)
.ub_install_stamp <- function(pkg_path) {
  p <- file.path(pkg_path, "Meta", "package.rds")
  if (!file.exists(p)) return(NA_real_)
  as.numeric(file.info(p)$mtime)
}

.ub_session_stale <- function(pkg_path = .ub_env$pkg_path, stamp = .ub_env$install_stamp) {
  if (is.null(pkg_path) || is.null(stamp) || is.na(stamp)) return(FALSE)
  now <- .ub_install_stamp(pkg_path)
  !is.na(now) && now != stamp
}

.ub_stale_message <- function() {
  paste0("This R session is running an older copy of universalblinder: the package was reinstalled after it was ",
         "loaded. Restart R (RStudio: Session > Restart R, Ctrl+Shift+F10), then run universalblinder::run_app() again. ",
         "(run_app.bat always starts a fresh R.)")
}
