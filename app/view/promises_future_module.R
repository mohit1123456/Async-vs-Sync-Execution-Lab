# app/view/promises_future_module.R
# ─────────────────────────────────────────────────────────────────────────────
# MODULE: Asynchronous — promises + future
# Library: {promises} + {future}
# Behaviour: Task runs in a background worker process. UI stays interactive.
# Pattern:   future({ ... }, seed = TRUE) %...>% { handle result }
#
# Design note: All logic inside future({}) is self-contained — no box-imported
# functions are serialized to workers. stats:: prefix is used explicitly.
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[
    actionButton, div, h4, hr, moduleServer, NS, observeEvent,
    p, reactiveVal, renderTable, renderText, tableOutput,
    tags, verbatimTextOutput
  ],
  future[future, plan, multisession],
  promises[`%...>%`, `%...!%`],
  app/logic/tasks[fmt_elapsed],
)

# One-time setup: start 2 persistent multisession workers
plan(multisession, workers = 2)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "method-card method-card--promises",

    div(class = "method-card__header method-card__header--success",
      div(class = "method-card__icon", "\U0001f7e1"),
      div(
        h4("promises + future", class = "method-card__title"),
        p("Async pipeline \u2014 Non-blocking", class = "method-card__subtitle")
      )
    ),

    div(class = "method-card__body",
      div(class = "method-card__info",
        tags$b("\u2705 UI stays responsive."),
        " The task runs in a separate R worker process."
      ),

      div(class = "method-card__how",
        tags$code("future({ ... }, seed=TRUE) %...>% { handle }"),
        tags$br(),
        "Uses ", tags$code("multisession"), " workers. Self-contained task logic."
      ),

      actionButton(
        ns("run"),
        "\u25b6 Run with promises + future",
        class = "btn-run btn-run--warning"
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
      status_msg("\u23f3 Running in background worker... UI is RESPONSIVE now.")
      output$result <- renderTable(NULL)

      dur <- duration()

      # ── NON-BLOCKING: future + promises ──────────────────────────────────
      # Self-contained: all logic lives inside future({}). No box closures
      # are serialized — only primitive values (dur) cross the boundary.
      # stats:: prefix used explicitly so workers find it without search path.
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
      }, seed = TRUE) %...>% {
        status_msg(paste0(
          "\u2705 Done in ", fmt_elapsed(.$elapsed),
          "  |  Worker PID: ", .$pid,
          "  |  Finished: ", .$end_t
        ))
        output$result <- renderTable(utils::head(.$payload, 5))
      } %...!% {
        status_msg(paste0("\u274c Error: ", conditionMessage(.)))
      }
      # ─────────────────────────────────────────────────────────────────────

      NULL
    })
  })
}
