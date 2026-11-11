#' Setup / About tab
#' @keywords internal
mod_about_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(bslib::card_header("Installation check"), shiny::tableOutput(ns("pkgs")), shiny::uiOutput(ns("versions"))),
    bslib::card(bslib::card_header("About"),
      shiny::p("Universal Blinder makes blinded, reversible copies of SDTM/ADaM pharmacokinetic datasets."),
      shiny::p("Template of a study folder: ", shiny::code(ub_template_path())),
      shiny::p("Start the app from R with ", shiny::code("universalblinder::run_app()"), " or double-click run_app.bat."),
      shiny::p("Claude API key: set ", shiny::code("ANTHROPIC_API_KEY"), " in your ", shiny::code(".Renviron"),
               " (open it with usethis::edit_r_environ()), or type it in the AI assistant tab for one session."),
      shiny::actionLink(ns("guide"), "Open the user guide"))
  )
}

#' @keywords internal
mod_about_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$pkgs <- shiny::renderTable({
      p <- c("haven", "jsonlite", "shiny", "bslib", "shinyFiles", "DT", "httr2", "readxl", "openxlsx", "commonmark")
      data.frame(Package = p, Installed = vapply(p, function(x) if (requireNamespace(x, quietly = TRUE))
        as.character(utils::packageVersion(x)) else "MISSING", ""))
    })
    output$versions <- shiny::renderUI(shiny::p("universalblinder ", as.character(utils::packageVersion("universalblinder")),
                                                " on ", R.version.string))
    shiny::observeEvent(input$guide, {
      g <- .ub_read_md(.ub_docs_path("USERGUIDE.md"))
      shiny::showModal(shiny::modalDialog(if (is.null(g)) "The user guide is not installed." else g,
                                          title = "User guide", size = "xl", easyClose = TRUE))
    })
  })
}
