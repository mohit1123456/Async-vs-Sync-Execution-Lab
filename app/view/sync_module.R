# app/view/sync_module.R
# ─────────────────────────────────────────────────────────────────────────────
# MODULE: Synchronous (Blocking) Execution
# Library: Base R — pure blocking call in the main Shiny process
# Behaviour: Freezes the entire UI until the task finishes.
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[
    actionButton, div, h4, hr, moduleServer, NS, observeEvent,
    p, reactiveVal, renderTable, renderText, tableOutput,
    tags, verbatimTextOutput
  ],
  app/logic/tasks[run_heavy_task, generate_data_payload, fmt_elapsed],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "method-card method-card--sync",

    div(class = "method-card__header method-card__header--danger",
      div(class = "method-card__icon", "\U0001f534"),
      div(
        h4("Synchronous", class = "method-card__title"),
        p("Base R \u2014 Blocking", class = "method-card__subtitle")
      )
    ),

    div(class = "method-card__body",
      div(class = "method-card__warning",
        tags$b("\u26a0 Blocks the entire UI."),
        " Click the global counter while this runs \u2014 it will NOT respond."
      ),

      div(class = "method-card__how",
        tags$code("data <- run_heavy_task(secs)"),
        tags$br(),
        "Executes in the main R process. No concurrency."
      ),

      actionButton(
        ns("run"),
        "\u25b6 Run Synchronously",
        class = "btn-run btn-run--danger"
      ),

      hr(),
      verbatimTextOutput(ns("status")),
      tableOutput(ns("result"))
    )
  )
}

#' @export
server <- function(id, duration) {
  moduleServer(id, function(input, output, session) {

    status_msg <- reactiveVal("Idle \u2014 press Run to start.")
    output$status <- renderText(status_msg())

    observeEvent(input$run, {
      status_msg("\u23f3 Running synchronously... UI is FROZEN now.")
      output$result <- renderTable(NULL)

      # ── BLOCKING CALL (runs in main R process) ────────────────────────────
      # Note: generate_data_payload runs HERE in main process — stats:: prefixes
      # in tasks.R ensure it works. No future serialization needed.
      task_meta <- run_heavy_task(duration(), task_type = "sync")
      payload   <- generate_data_payload(20)
      # ─────────────────────────────────────────────────────────────────────

      status_msg(paste0(
        "\u2705 Done in ", fmt_elapsed(task_meta$elapsed_secs),
        "  |  PID: ", task_meta$pid,
        "  |  Finished: ", task_meta$end_time
      ))

      output$result <- renderTable(utils::head(payload, 5))
    })
  })
}
