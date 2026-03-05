# app/view/mirai_module.R
# ─────────────────────────────────────────────────────────────────────────────
# MODULE: Asynchronous — mirai (event-driven, minimal async)
# Library: {mirai} — if installed; otherwise shows installation instructions
# Behaviour: mirai uses persistent daemon workers (not spawned per request).
#            It is the lightest-weight, fastest async backend for Shiny.
#            ExtendedTask (Shiny ≥ 1.8.1) pairs best with mirai.
# Pattern:   mirai::daemons(n) → mirai({ ... }) → then()
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[
    actionButton, code, div, h4, h5, hr, moduleServer, NS, observeEvent,
    p, reactiveVal, renderTable, renderText, tableOutput, tags,
    verbatimTextOutput, wellPanel
  ],
  app/logic/tasks[fmt_elapsed],
)

HAS_MIRAI <- requireNamespace("mirai", quietly = TRUE)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "method-card method-card--mirai",

    div(class = "method-card__header method-card__header--orange",
      div(class = "method-card__icon", "⚡"),
      div(
        h4("mirai — Event-driven Async", class = "method-card__title"),
        p("Persistent Daemons — Fastest Backend", class = "method-card__subtitle")
      )
    ),

    div(class = "method-card__body",

      if (!HAS_MIRAI) {
        div(class = "method-card__install",
          p(tags$b("📦 mirai is NOT installed.")),
          p("Install it by running in your R console:"),
          tags$pre(class = "code-block",
            'install.packages("mirai")\n',
            '# Then restart R and re-run the app'
          ),
          p(class = "text-muted",
            "mirai is the recommended backend for ExtendedTask in Shiny 1.8.1+.",
            tags$br(),
            "It uses persistent daemon workers (no spawn overhead per request)."
          ),
          hr(),
          h5("Conceptual code (requires mirai):"),
          tags$pre(class = "code-block",
            "library(mirai)\n",
            "# Start 4 persistent daemon workers\n",
            "daemons(4)\n\n",
            "# Submit a task — returns immediately\n",
            "m <- mirai({\n",
            "  Sys.sleep(dur)\n",
            "  list(pid = Sys.getpid(), done = TRUE)\n",
            "}, dur = 3)\n\n",
            "# Attach to Shiny via promises\n",
            "promises::as.promise(m) |>\n",
            "  then(function(result) {\n",
            "    cat('Done on PID', result$pid)\n",
            "  })"
          )
        )
      } else {
        div(
          div(class = "method-card__info",
            tags$b("✅ mirai is installed!"),
            " Using persistent daemon workers — fastest async backend."
          ),
          div(class = "method-card__how",
            tags$code("daemons(4) → mirai({ ... }) → as.promise() |> then()"),
            tags$br(),
            "Daemons are reused across tasks — zero spawn overhead."
          ),
          actionButton(
            ns("run"),
            "▶ Run with mirai",
            class = "btn-run btn-run--orange"
          ),
          hr(),
          verbatimTextOutput(ns("status")),
          tableOutput(ns("result"))
        )
      }
    )
  )
}

#' @export
server <- function(id, duration) {
  moduleServer(id, function(input, output, session) {

    status_msg <- reactiveVal(
      if (HAS_MIRAI) "Idle — press Run to start."
      else "mirai not installed — showing conceptual demo only."
    )

    output$status <- renderText(status_msg())

    if (HAS_MIRAI) {
      # Dynamically load mirai only if available
      box::use(mirai[mirai, daemons])
      box::use(promises[as.promise, then, `%...!%`])

      # Start 2 persistent background daemons (done once per module init)
      daemons(2)

      observeEvent(input$run, {
        status_msg("⏳ mirai task dispatched to daemon... UI is RESPONSIVE now.")
        output$result <- renderTable(NULL)

        dur <- duration()

        # ── NON-BLOCKING: mirai ───────────────────────────────────────────
        m <- mirai({
          Sys.sleep(dur)
          list(
            pid     = Sys.getpid(),
            elapsed = dur,
            end_t   = format(Sys.time(), "%H:%M:%S"),
            data    = data.frame(
              id    = seq_len(20),
              value = round(stats::rnorm(20, 100, 15), 2),
              cat   = base::sample(c("A","B","C"), 20, replace = TRUE)
            )
          )
        }, dur = dur)
        # ─────────────────────────────────────────────────────────────────

        as.promise(m) |>
          then(
            onFulfilled = function(result) {
              status_msg(paste0(
                "✅ Done in ", fmt_elapsed(result$elapsed),
                "  |  Daemon PID: ", result$pid,
                "  |  Finished: ", result$end_t
              ))
              output$result <- renderTable(utils::head(result$data, 5))
            },
            onRejected = function(e) {
              status_msg(paste0("❌ Error: ", conditionMessage(e)))
            }
          )

        NULL
      })
    }
  })
}
