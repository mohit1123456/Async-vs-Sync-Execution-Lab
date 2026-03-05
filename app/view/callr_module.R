# app/view/callr_module.R
# ─────────────────────────────────────────────────────────────────────────────
# MODULE: Asynchronous — callr (background R sessions)
# Library: {callr}
# Behaviour: Spawns a completely fresh background R session, runs code there,
#            then polls for the result using promises. Great for isolating
#            side-effects (e.g. loading conflicting packages).
# Pattern:   callr::r_bg() → poll with promises::promise() + invalidateLater
#
# NOTE: callr's r_bg() func runs in a truly fresh R session — stats package
# IS loaded there by default, so stats::rnorm etc. work fine.
# We still use explicit stats:: for clarity and consistency.
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[
    actionButton, div, h4, hr, invalidateLater, isolate, moduleServer,
    NS, observe, observeEvent, p, reactiveVal, renderTable, renderText,
    tableOutput, tags, verbatimTextOutput
  ],
  callr[r_bg],
  promises[promise, `%...>%`, `%...!%`],
  app/logic/tasks[fmt_elapsed],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "method-card method-card--callr",

    div(class = "method-card__header method-card__header--info",
      div(class = "method-card__icon", "\U0001f7e2"),
      div(
        h4("callr (r_bg)", class = "method-card__title"),
        p("Fresh R Session \u2014 Non-blocking", class = "method-card__subtitle")
      )
    ),

    div(class = "method-card__body",
      div(class = "method-card__info",
        tags$b("\u2705 Isolated R session."),
        " Spawns a fresh background R process. Perfect for isolating side-effects."
      ),

      div(class = "method-card__how",
        tags$code("callr::r_bg(func, args) + promise() polling"),
        tags$br(),
        "The background session has NO shared state with the Shiny app."
      ),

      actionButton(
        ns("run"),
        "\u25b6 Run with callr::r_bg()",
        class = "btn-run btn-run--info"
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
    bg_process <- reactiveVal(NULL)   # holds the callr background process

    output$status <- renderText(status_msg())

    observeEvent(input$run, {
      status_msg("\u23f3 Spawned background R session... UI is RESPONSIVE now.")
      output$result <- renderTable(NULL)

      # Kill any previously running background process
      prev <- isolate(bg_process())
      if (!is.null(prev) && prev$is_alive()) prev$kill()

      dur <- duration()

      # ── NON-BLOCKING: callr::r_bg ─────────────────────────────────────────
      # r_bg() starts a FRESH R session (full clean environment).
      # stats package is always loaded there — rnorm/sample work fine.
      bg <- r_bg(
        func = function(duration_secs) {
          Sys.sleep(duration_secs)
          list(
            pid     = Sys.getpid(),
            elapsed = duration_secs,
            end_t   = format(Sys.time(), "%H:%M:%S"),
            data    = data.frame(
              id    = seq_len(20),
              value = round(stats::rnorm(20, 100, 15), 2),
              cat   = base::sample(c("A", "B", "C"), 20, replace = TRUE)
            )
          )
        },
        args      = list(duration_secs = dur),
        supervise = TRUE   # kill process if Shiny session ends
      )
      bg_process(bg)
      # ─────────────────────────────────────────────────────────────────────

      # Resolve the callr process into a promise using polling via observe()
      result_promise <- promise(function(resolve, reject) {
        # Use observe() + invalidateLater() to poll without blocking
        obs <- observe({
          proc <- isolate(bg_process())
          if (is.null(proc)) {
            reject("No background process found.")
          } else if (proc$is_alive()) {
            invalidateLater(400, session)   # re-check every 400ms
          } else {
            tryCatch(
              resolve(proc$get_result()),
              error = function(e) reject(conditionMessage(e))
            )
          }
        })
      })

      result_promise %...>% {
        status_msg(paste0(
          "\u2705 Done in ", fmt_elapsed(.$elapsed),
          "  |  Session PID: ", .$pid,
          "  |  Finished: ", .$end_t
        ))
        output$result <- renderTable(utils::head(.$data, 5))
      } %...!% {
        status_msg(paste0("\u274c Error: ", .))
      }

      NULL
    })
  })
}
