# app/view/extended_task_module.R
# ─────────────────────────────────────────────────────────────────────────────
# MODULE: Asynchronous — ExtendedTask
# Library: {shiny} 1.8.1+
# Behaviour: Uses the new ExtendedTask class for background operations.
#            Pairs perfectly with {bslib}'s input_task_button().
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[
    div, h4, hr, moduleServer, NS, observe, observeEvent,
    p, reactiveVal, renderTable, renderText, tableOutput,
    tags, verbatimTextOutput, ExtendedTask
  ],
  bslib[input_task_button],
  future[future, plan, multisession],
  app/logic/tasks[fmt_elapsed],
)

# Start workers if not already started
plan(multisession, workers = 2)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "method-card method-card--extended",

    div(class = "method-card__header method-card__header--success",
      div(class = "method-card__icon", "\U0001f7e2"),
      div(
        h4("Shiny ExtendedTask", class = "method-card__title"),
        p("Native Shiny Async \u2014 New in 1.8.1", class = "method-card__subtitle")
      )
    ),

    div(class = "method-card__body",
      div(class = "method-card__info",
        tags$b("\u2705 State-aware Button."),
        " Automatically shows a spinner and disables while the task runs."
      ),

      div(class = "method-card__how",
        tags$code("task <- ExtendedTask$new(func)"),
        tags$br(),
        "The modern way to handle single-process async tasks in Shiny."
      ),

      input_task_button(
        ns("run"),
        "\u25b6 Run via ExtendedTask"
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

    # Define the ExtendedTask
    # It takes a function that returns a promise
    task <- ExtendedTask$new(function(dur) {
      future({
        start <- proc.time()
        Sys.sleep(dur)
        elapsed <- round((proc.time() - start)[["elapsed"]], 2)

        n <- 20L
        list(
          elapsed = elapsed,
          pid     = Sys.getpid(),
          end_t   = format(Sys.time(), "%H:%M:%S"),
          payload = data.frame(
            id       = seq_len(n),
            value    = round(stats::rnorm(n, mean = 100, sd = 15), 2),
            category = base::sample(c("Alpha","Beta","Gamma","Delta"), n, replace = TRUE),
            flag     = base::sample(c(TRUE, FALSE), n, replace = TRUE),
            stringsAsFactors = FALSE
          )
        )
      }, seed = TRUE)
    })

    # Trigger the task
    observeEvent(input$run, {
      status_msg("\u23f3 ExtendedTask dispatched... button is disabled.")
      output$result <- renderTable(NULL)
      task$invoke(duration())
    })

    # Handle the result
    output$result <- renderTable({
      res <- task$result() # This is reactive, it returns the value when done
      utils::head(res$payload, 5)
    })

    observe({
      res <- task$result()
      status_msg(paste0(
        "\u2705 Done in ", fmt_elapsed(res$elapsed),
        "  |  Worker PID: ", res$pid,
        "  |  Finished: ", res$end_t
      ))
    })

  })
}
