#' The app's user interface
#' @keywords internal
app_ui <- function() {
  bslib::page_navbar(
    id = "main",
    # a normal, scrolling page: with fillable panels every tab is squeezed into the window height and content
    # below it (answers, suggestion tables, Apply buttons) hides behind a small scrollbar inside the card
    fillable = FALSE,
    title = shiny::span(shiny::strong("Universal Blinder")),
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly", base_font = "Segoe UI, Arial, sans-serif"),
    header = shiny::tagList(
      shiny::tags$style(shiny::HTML(paste(
        ".ub-study{font-size:.9rem;padding:.25rem .75rem;background:#eef2f7;border-bottom:1px solid #ccd}",
        ".ub-step{border-left:4px solid #ccc;padding:.4rem .8rem;margin:.4rem 0}",
        ".ub-step.done{border-color:#198754}.ub-step.todo{border-color:#ffc107}.ub-step.fail{border-color:#dc3545}",
        "pre.ub-log{max-height:420px;overflow:auto;background:#f6f6f6;font-size:12px}",
        "table.dataTable td{font-size:13px}",
        # tables take the height of their rows (DT widgets default to a fixed 400px box that hides rows and
        # buttons behind a scrollbar inside the card); the page scrolls instead
        ".datatables.html-widget{height:auto !important;flex:0 0 auto !important}"))),
      shiny::uiOutput("study_banner")),
    bslib::nav_panel("Workflow", value = "workflow", mod_workflow_ui("workflow")),
    bslib::nav_panel("Study", value = "study", mod_study_ui("study")),
    bslib::nav_panel("Prepare PC", value = "prepare_pc", mod_prepare_pc_ui("prepare_pc")),
    bslib::nav_panel("Study settings", value = "settings", mod_settings_ui("settings")),
    bslib::nav_panel("Codebook review", value = "codebook", mod_codebook_ui("codebook")),
    bslib::nav_panel("Blind & QC", value = "blind", mod_blind_ui("blind")),
    bslib::nav_panel("Retrieve", value = "retrieve", mod_retrieve_ui("retrieve")),
    bslib::nav_panel("AI assistant", value = "ai", mod_ai_ui("ai")),
    bslib::nav_spacer(),
    bslib::nav_panel("Setup / About", value = "about", mod_about_ui("about"))
  )
}
