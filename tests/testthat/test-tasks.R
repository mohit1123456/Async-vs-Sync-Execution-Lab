# tests/testthat/test-tasks.R
# Unit tests for app/logic/tasks.R  — pure logic, no Shiny required

box::use(
  testthat[describe, expect_equal, expect_gt, expect_true, expect_type, it, test_that],
  app/logic/tasks[run_heavy_task, generate_data_payload, fmt_elapsed],
)

test_that("run_heavy_task returns correct structure", {
  result <- run_heavy_task(duration_secs = 0.1, task_type = "test")

  expect_type(result, "list")  # data.frame is a list
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1L)
  expect_equal(result$task_type, "test")
  expect_equal(result$rows_generated, 50L)
  expect_gt(result$elapsed_secs, 0)
})

test_that("generate_data_payload produces correct dimensions", {
  df <- generate_data_payload(n = 30)

  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 30L)
  expect_equal(ncol(df), 4L)
  expect_equal(names(df), c("id", "value", "category", "flag"))
  expect_true(all(df$category %in% c("Alpha", "Beta", "Gamma", "Delta")))
})

test_that("fmt_elapsed formats seconds correctly", {
  expect_equal(fmt_elapsed(3.215), "3.22 sec")
  expect_equal(fmt_elapsed(0),     "0 sec")
  expect_equal(fmt_elapsed(NULL),  "—")
  expect_equal(fmt_elapsed(NA),    "—")
})
