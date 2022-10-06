test_that("has nice logo", {
  local_reproducible_output(crayon = TRUE)
  expect_snapshot(tidyverse_logo())
})
