#' The app's server
#' @param input,output,session Shiny
#' @keywords internal
app_server <- function(input, output, session) {
  state <- shiny::reactiveValues(ub_dir = NULL, study_dir = NULL, tick = 0, ai_context = NULL)
  state$refresh <- function() state$tick <- shiny::isolate(state$tick) + 1
  # step: a sub-tab of that tab (Blind & QC has "1. Pre-flight", "2. Run blinding", "3. QC result")
  state$goto <- function(tab, step = NULL) {
    bslib::nav_select("main", tab, session = session)
    if (!is.null(step)) bslib::nav_select(paste0(tab, "-steps"), step, session = session)
  }
  if (.ub_session_stale()) shiny::showNotification(.ub_stale_message(), type = "error", duration = NULL)

  output$study_banner <- shiny::renderUI({
    if (is.null(state$ub_dir)) return(shiny::div(class = "ub-study", "No study selected: start in the 'Study' tab."))
    shiny::div(class = "ub-study", shiny::strong("Study folder: "), shiny::code(state$ub_dir),
               shiny::span(class = "ms-3 text-danger", "input\\ and private\\ hold real data: never share them."))
  })

  mod_workflow_server("workflow", state)
  mod_study_server("study", state)
  mod_prepare_pc_server("prepare_pc", state)
  mod_settings_server("settings", state)
  mod_codebook_server("codebook", state)
  mod_blind_server("blind", state)
  mod_retrieve_server("retrieve", state)
  mod_ai_server("ai", state)
  mod_about_server("about", state)
}
