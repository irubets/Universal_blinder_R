#' Start the Universal Blinder app
#'
#' Opens the app in the web browser. It runs on this computer only: no data leaves it unless you use the
#' AI assistant tab, which follows the rules shown there.
#'
#' The app always uses the same address, `http://127.0.0.1:3838`, so a bookmark keeps working. If that port is
#' taken (for example the app is already open in another R session), a free port is chosen and a message says so.
#' @param launch.browser open the browser (default TRUE)
#' @param port port to use; default 3838 (or the `shiny.port` option if set)
#' @export
run_app <- function(launch.browser = TRUE, port = getOption("shiny.port", 3838L)) {
  if (.ub_session_stale()) warning(.ub_stale_message(), call. = FALSE, immediate. = TRUE)
  shiny::addResourcePath("ubwww", system.file("app", "www", package = "universalblinder"))
  app <- shiny::shinyApp(ui = app_ui(), server = app_server)
  if (!is.null(port) && !.ub_port_free(port)) {
    message(sprintf(paste0("Port %s is already in use (is the app already open at http://127.0.0.1:%s ?). ",
                           "Using a free port instead; the address is shown below."), port, port))
    port <- NULL
  }
  shiny::runApp(app, launch.browser = launch.browser, port = port, host = "127.0.0.1")
}

# TRUE if nothing listens on this local port. Two checks: connecting succeeds if something answers on
# 127.0.0.1 (on Windows a bind test alone misses a server bound to 127.0.0.1 only), and binding fails if
# another program holds the port on all addresses.
.ub_port_free <- function(port) {
  port <- as.integer(port)
  con <- suppressWarnings(tryCatch(socketConnection("127.0.0.1", port, open = "r+b", blocking = TRUE, timeout = 1),
                                   error = function(e) NULL))
  if (!is.null(con)) { close(con); return(FALSE) }
  s <- tryCatch(serverSocket(port), error = function(e) NULL, warning = function(w) NULL)
  if (is.null(s)) return(FALSE)
  close(s)
  TRUE
}
