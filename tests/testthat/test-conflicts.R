test_that("useful conflicts message", {
  expect_snapshot({
    tidyverse_conflicts(c("base", "stats"))
  })
})
