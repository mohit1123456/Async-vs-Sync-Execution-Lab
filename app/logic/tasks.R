# app/logic/tasks.R
# Pure R functions for long-running tasks — no Shiny reactivity here.
# These are used by all async/sync modules as the shared "work" to execute.
#
# IMPORTANT: Use fully-qualified package prefixes (stats::rnorm, base::Sys.sleep)
# for ALL non-base functions. This ensures closures serialized to future/callr
# worker processes can still find these functions — worker envs don't inherit
# the box module's environment chain.

#' Simulate a CPU-bound or I/O-bound long-running task
#'
#' @param duration_secs Number of seconds to sleep (simulate work)
#' @param task_type     A label describing the type of task
#' @return A data.frame with task results and metadata
#' @export
run_heavy_task <- function(duration_secs = 3, task_type = "generic") {
  start_time <- proc.time()
  Sys.sleep(duration_secs)
  end_time <- proc.time()

  elapsed <- round((end_time - start_time)[["elapsed"]], 2)

  data.frame(
    task_type      = task_type,
    pid            = Sys.getpid(),
    start_time     = format(Sys.time() - duration_secs, "%H:%M:%S"),
    end_time       = format(Sys.time(), "%H:%M:%S"),
    elapsed_secs   = elapsed,
    rows_generated = 50L,
    stringsAsFactors = FALSE
  )
}

#' Generate a random data payload (simulates a "data fetch")
#' Uses stats:: prefix explicitly so this survives future/callr serialization.
#'
#' @param n Number of rows
#' @return A data.frame
#' @export
generate_data_payload <- function(n = 50) {
  # Fully qualified — stats:: ensures availability in serialized worker envs
  values    <- stats::rnorm(n, mean = 100, sd = 15)
  categories <- base::sample(c("Alpha", "Beta", "Gamma", "Delta"), n, replace = TRUE)
  flags      <- base::sample(c(TRUE, FALSE), n, replace = TRUE)

  data.frame(
    id       = seq_len(n),
    value    = round(values, 2),
    category = categories,
    flag     = flags,
    stringsAsFactors = FALSE
  )
}

#' Format elapsed time nicely
#'
#' @param secs Seconds (numeric)
#' @return Character string like "3.21 sec"
#' @export
fmt_elapsed <- function(secs) {
  if (is.null(secs) || is.na(secs)) return("\u2014")
  paste0(round(secs, 2), " sec")
}
