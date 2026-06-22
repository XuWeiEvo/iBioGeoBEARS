#' Launch the iBiogeobears Shiny application
#'
#' @param config Optional path to an `analysis.yml` file.
#' @param output_dir Optional workflow output directory override.
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return The value returned by [shiny::runApp()].
#' @export
launch_app <- function(config = NULL, output_dir = NULL, launch.browser = TRUE, ...) {
  check_shiny_available()
  app <- create_iBGB_shiny_app(config = config, output_dir = output_dir)
  shiny::runApp(app, launch.browser = launch.browser, ...)
}

create_iBGB_shiny_app <- function(config = NULL, output_dir = NULL) {
  check_shiny_available()

  default_config <- config %||% system.file("templates", "analysis.yml", package = "iBiogeobears")
  default_output <- output_dir %||% ""

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::tags$head(
        shiny::tags$style(shiny::HTML(
          ".container-fluid{max-width:1180px} .well{border-radius:4px} ",
          ".btn{border-radius:4px} .ibgb-status{font-weight:600;margin:8px 0}"
        ))
      ),
      shiny::titlePanel("iBiogeobears"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::textInput("config_path", "analysis.yml", value = default_config),
          shiny::textInput("output_dir", "Output directory", value = default_output),
          shiny::checkboxInput("dry_run", "Dry run", value = TRUE),
          shiny::checkboxInput("require_biogeobears", "Require BioGeoBEARS", value = FALSE),
          shiny::checkboxInput("force", "Force execution after validation failure", value = FALSE),
          shiny::selectInput("report_format", "Report format", choices = c("source", "html", "pdf"), selected = "html"),
          shiny::actionButton("validate", "Validate"),
          shiny::actionButton("run", "Run workflow"),
          shiny::actionButton("render_report", "Render report"),
          shiny::actionButton("bundle", "Bundle results")
        ),
        shiny::mainPanel(
          shiny::uiOutput("status"),
          shiny::tabsetPanel(
            shiny::tabPanel("Validation", shiny::tableOutput("validation_table")),
            shiny::tabPanel("Models", shiny::tableOutput("model_table")),
            shiny::tabPanel("Manifest", shiny::tableOutput("manifest_table")),
            shiny::tabPanel("Paths", shiny::verbatimTextOutput("paths_text")),
            shiny::tabPanel("Messages", shiny::verbatimTextOutput("messages_text"))
          )
        )
      )
    ),
    server = function(input, output, session) {
      state <- shiny::reactiveValues(
        result = NULL,
        validation = NULL,
        model_table = NULL,
        manifest = NULL,
        report = NULL,
        bundle = NULL,
        message = "Ready."
      )

      current_output_dir <- shiny::reactive({
        value <- trimws(input$output_dir %||% "")
        if (nzchar(value)) value else NULL
      })

      shiny::observeEvent(input$validate, {
        shiny::withProgress(message = "Validating", value = 0, {
          cfg <- read_config(input$config_path)
          if (!is.null(current_output_dir())) {
            cfg$project$output_dir <- current_output_dir()
          }
          state$validation <- validate_inputs(cfg)
          state$model_table <- planned_model_table(cfg)
          state$message <- if (all(state$validation$ok)) "Validation passed." else "Validation failed."
          shiny::incProgress(1)
        })
      })

      shiny::observeEvent(input$run, {
        shiny::withProgress(message = "Running workflow", value = 0, {
          result <- run_workflow(
            config = input$config_path,
            output_dir = current_output_dir(),
            dry_run = isTRUE(input$dry_run),
            require_biogeobears = isTRUE(input$require_biogeobears),
            force = isTRUE(input$force)
          )
          state$result <- result
          state$validation <- result$validation
          state$model_table <- result$model_run_status
          state$manifest <- result$workflow_manifest
          state$message <- if (isTRUE(result$dry_run)) "Dry run completed." else "Workflow completed."
          shiny::incProgress(1)
        })
      })

      shiny::observeEvent(input$render_report, {
        require_workflow_result(state$result)
        shiny::withProgress(message = "Rendering report", value = 0, {
          state$report <- render_report(state$result, format = input$report_format)
          state$manifest <- create_workflow_manifest(state$result, write = TRUE)
          state$message <- paste("Report:", state$report)
          shiny::incProgress(1)
        })
      })

      shiny::observeEvent(input$bundle, {
        require_workflow_result(state$result)
        shiny::withProgress(message = "Bundling results", value = 0, {
          state$bundle <- bundle_results(state$result, overwrite = TRUE)
          state$manifest <- create_workflow_manifest(state$result, write = TRUE)
          state$message <- paste("Bundle:", state$bundle)
          shiny::incProgress(1)
        })
      })

      output$status <- shiny::renderUI({
        shiny::tags$div(class = "ibgb-status", state$message)
      })

      output$validation_table <- shiny::renderTable({
        state$validation
      }, striped = TRUE, bordered = TRUE, na = "")

      output$model_table <- shiny::renderTable({
        table_head(state$model_table, 20L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$manifest_table <- shiny::renderTable({
        table_head(state$manifest, 30L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$paths_text <- shiny::renderText({
        if (is.null(state$result)) {
          return("")
        }
        paste(utils::capture.output(utils::str(state$result$project_paths)), collapse = "\n")
      })

      output$messages_text <- shiny::renderText({
        paste(
          state$message,
          if (!is.null(state$report)) paste("Report:", state$report) else NULL,
          if (!is.null(state$bundle)) paste("Bundle:", state$bundle) else NULL,
          sep = "\n"
        )
      })
    }
  )
}

check_shiny_available <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The shiny package is required to launch the GUI. Install it with install.packages('shiny').", call. = FALSE)
  }
  invisible(TRUE)
}

planned_model_table <- function(config) {
  models <- config$models$run %||% valid_models()
  data.frame(
    model = models,
    model_family = model_family(models),
    has_j = is_j_model(models),
    status = "planned",
    stringsAsFactors = FALSE
  )
}

require_workflow_result <- function(result) {
  if (is.null(result)) {
    stop("Run the workflow before requesting this action.", call. = FALSE)
  }
  invisible(TRUE)
}

table_head <- function(x, n = 20L) {
  if (is.null(x)) {
    return(data.frame())
  }
  utils::head(x, n)
}
