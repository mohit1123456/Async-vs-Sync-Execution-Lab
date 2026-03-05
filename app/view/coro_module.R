# app/view/coro_module.R
# ─────────────────────────────────────────────────────────────────────────────
# MODULE: Asynchronous — coro (async / await)
# Library: {coro} + {future} + {promises}
# Pattern: coro::async(function() { result <- await(future({ ... })) })
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[
    actionButton, div, h4, hr, moduleServer, NS, observeEvent,
    p, reactiveVal, renderTable, renderText, tableOutput,
    tags, verbatimTextOutput
  ],
  future[future],
  promises[`%...>%`, `%...!%`],
  coro[async, await],
  app/logic/tasks[fmt_elapsed],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "method-card method-card--coro",

    div(class = "method-card__header method-card__header--purple",
      div(class = "method-card__icon", "\U0001f535"),
      div(
        h4("coro \u2014 async / await", class = "method-card__title"),
        p("Coroutines \u2014 Sequential Async Style", class = "method-card__subtitle")
      )
    ),

    div(class = "method-card__body",
      div(class = "method-card__info",
        tags$b("\u2705 Sequential-looking async code."),
        " ", tags$code("await()"), " suspends the coroutine, not the entire session."
      ),

      div(class = "method-card__how",
        tags$code("async(function() { result <- await(future({...})) })"),
        tags$br(),
        "coro makes async code look synchronous \u2014 no callback nesting."
      ),

      actionButton(
        ns("run"),
        "\u25b6 Run with coro::async()",
        class = "btn-run btn-run--purple"
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
      status_msg("\u23f3 Coroutine suspended at await()... UI is RESPONSIVE now.")
      output$result <- renderTable(NULL)

      dur <- duration()

      # ── coro::async + await ───────────────────────────────────────────────
      # The coroutine runs sequentially from the developer's POV but
      # doesn't block the session — await() yields control back to Shiny.
      run_coro <- async(function() {
        result <- await(
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
        )
        result
      })

      run_coro() %...>% {
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
