# app/main.R
# ─────────────────────────────────────────────────────────────────────────────
# Rhino App Entry Point
# Project: Async vs Sync Testing Lab
# Purpose: Compare synchronous and asynchronous execution patterns in Shiny
#
# Libraries tested:
#   🔴 Sync         — Base R blocking
#   🟡 promises     — {promises} + {future} pipeline
#   🟢 callr        — {callr} r_bg() isolated sessions
#   🔵 coro         — {coro} async/await coroutines
#   ⚡ mirai        — {mirai} persistent daemon workers (install separately)
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[
    bootstrapPage, column, div, fluidPage, fluidRow, h1, h2, hr,
    moduleServer, NS, p, renderUI, shinyApp, span, tags, uiOutput
  ],
  bslib[bs_theme, page_sidebar, sidebar],

  # View modules — one per async/sync method
  app/view/ui_tester_module,
  app/view/sync_module,
  app/view/promises_future_module,
  app/view/extended_task_module,
  app/view/callr_module,
  app/view/coro_module,
  app/view/mirai_module,
)

# ─── UI ───────────────────────────────────────────────────────────────────────

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    title = div(
      class = "app-title",
      span(class = "app-title__icon", "⚗️"),
      span(class = "app-title__text", "Async vs Sync Lab"),
      span(class = "app-title__badge", "Rhino + R")
    ),
    theme = bs_theme(
      version = 5,
      bootswatch = "darkly",
      primary   = "#7c3aed",
      secondary = "#6b7280",
      success   = "#10b981",
      info      = "#3b82f6",
      warning   = "#f59e0b",
      danger    = "#ef4444"
    ),
    window_title = "Async vs Sync Lab | Rhino",

    # ── Sidebar ──────────────────────────────────────────────────────────────
    sidebar = sidebar(
      width = 280,
      ui_tester_module$ui(ns("tester"))
    ),

    # ── Main Content ─────────────────────────────────────────────────────────
    div(class = "lab-container",

      # Page header
      div(class = "lab-header",
        h1("Async vs Sync Execution Lab", class = "lab-header__title"),
        p(class = "lab-header__desc",
          "Test and compare synchronous (blocking) vs. asynchronous (non-blocking) ",
          "execution patterns using ", tags$b("promises"), ", ", tags$b("future"), ", ",
          tags$b("callr"), ", ", tags$b("coro"), ", and ", tags$b("mirai"), ".",
          " All modules follow the ", tags$b("Rhino"), " project structure with ",
          tags$code("box::use()"), " imports and isolated UI/Logic/View layers."
        )
      ),

      hr(class = "lab-divider"),

      # ── Row 1: Sync vs Promises+Future ───────────────────────────────────
      div(class = "methods-section",
        h2("⚡ Execution Methods", class = "section-title"),

        div(class = "methods-grid",

          # 1. Synchronous (Blocking)
          sync_module$ui(ns("sync")),

          # 2. promises + future
          promises_future_module$ui(ns("promises_future"))
        ),

        div(class = "methods-grid",
          # 3. ExtendedTask
          extended_task_module$ui(ns("extended")),

          # 4. callr::r_bg()
          callr_module$ui(ns("callr"))
        ),

        div(class = "methods-grid",
          # 5. coro::async()
          coro_module$ui(ns("coro"))
        ),

        # 5. mirai (full-width — shows install note if not present)
        div(class = "methods-grid methods-grid--single",
          mirai_module$ui(ns("mirai"))
        )
      ),

      hr(class = "lab-divider"),

      # ── Concept Reference ─────────────────────────────────────────────────
      div(class = "reference-section",
        h2("📚 Concept Reference", class = "section-title"),
        div(class = "reference-grid",

          div(class = "ref-card",
            div(class = "ref-card__title", "🔴 Synchronous"),
            tags$ul(
              tags$li(tags$code("data <- slow_func()"), " — blocks all users"),
              tags$li("Runs in main Shiny R process"),
              tags$li("Zero setup required"),
              tags$li("Use for: tasks < 0.5s or single-user apps")
            )
          ),

          div(class = "ref-card",
            div(class = "ref-card__title", "🟡 promises + future"),
            tags$ul(
              tags$li(tags$code("future({ ... }) %...>% { ... }")),
              tags$li("Spawns worker pool at startup"),
              tags$li("Workers are persistent (", tags$code("multisession"), ")"),
              tags$li("Use for: CPU-bound tasks, multi-user apps")
            )
          ),

          div(class = "ref-card",
            div(class = "ref-card__title", "🟢 callr::r_bg()"),
            tags$ul(
              tags$li(tags$code("r_bg(func, args) → promise()")),
              tags$li("Each call spawns a FRESH R session"),
              tags$li("No shared state — total isolation"),
              tags$li("Use for: tasks needing clean environment")
            )
          ),

          div(class = "ref-card",
            div(class = "ref-card__title", "🔵 coro::async()"),
            tags$ul(
              tags$li(tags$code("async(function() { await(...) })")),
              tags$li("Sequential-looking async code"),
              tags$li("Wraps promise-based operations cleanly"),
              tags$li("Use for: complex async flows with multiple awaits")
            )
          ),

          div(class = "ref-card",
            div(class = "ref-card__title", "⚡ mirai"),
            tags$ul(
              tags$li(tags$code("daemons(n) → mirai({ ... })")),
              tags$li("Persistent, event-driven workers"),
              tags$li("Zero poll overhead — truly event-driven"),
              tags$li("Use for: high-concurrency, low-latency tasks")
            )
          )
        )
      )
    )
  )
}

# ─── Server ───────────────────────────────────────────────────────────────────

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Tester module returns duration reactive + registers click counter
    tester <- ui_tester_module$server("tester")

    # Shared duration reactive passed to all child modules
    dur <- tester$duration

    # Initialise all method servers
    sync_module$server("sync",                  duration = dur)
    promises_future_module$server("promises_future", duration = dur)
    extended_task_module$server("extended",           duration = dur)
    callr_module$server("callr",                duration = dur)
    coro_module$server("coro",                  duration = dur)
    mirai_module$server("mirai",                duration = dur)
  })
}
