# app/view/ui_tester_module.R
# ─────────────────────────────────────────────────────────────────────────────
# MODULE: UI Responsiveness Tester
# A sidebar panel with a click counter to test whether the UI is blocked.
# If the counter updates immediately during a task → async is working.
# If the counter only updates AFTER a task → the UI was blocked (sync).
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[
    actionButton, div, h4, hr, moduleServer, NS, observeEvent,
    p, reactiveVal, renderText, sliderInput, tags, textOutput
  ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "tester-panel",

    div(class = "tester-panel__header",
      h4("🧪 UI Responsiveness Tester")
    ),

    div(class = "tester-panel__body",
      p(class = "tester-panel__desc",
        "Click the button below rapidly ", tags$b("while a task is running."),
        " If the counter updates immediately → UI is responsive (async). ",
        "If it only updates after the task finishes → UI was blocked (sync)."
      ),

      sliderInput(
        ns("duration"),
        "⏱ Task Duration (seconds)",
        min = 1, max = 15, value = 5, step = 1
      ),

      hr(),

      actionButton(
        ns("click"),
        "👆 Click Me! (Responsiveness Test)",
        class = "btn-click-test",
        width = "100%"
      ),

      div(class = "tester-panel__counter",
        textOutput(ns("counter"))
      ),

      hr(),

      div(class = "tester-panel__legend",
        div(class = "legend-item",
          div(class = "legend-dot legend-dot--danger"), " Sync — Blocks UI"
        ),
        div(class = "legend-item",
          div(class = "legend-dot legend-dot--warning"), " promises + future"
        ),
        div(class = "legend-item",
          div(class = "legend-dot legend-dot--info"), " callr::r_bg()"
        ),
        div(class = "legend-item",
          div(class = "legend-dot legend-dot--purple"), " coro::async()"
        ),
        div(class = "legend-item",
          div(class = "legend-dot legend-dot--orange"), " mirai"
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    click_count <- reactiveVal(0)

    observeEvent(input$click, {
      click_count(click_count() + 1)
    })

    output$counter <- renderText({
      paste0("Click count: ", click_count(),
             if (click_count() == 0) "" else " ✅")
    })

    # Return duration as reactive for other modules to use
    list(duration = shiny::reactive(input$duration))
  })
}
