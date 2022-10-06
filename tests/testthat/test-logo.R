test_that("has nice logo", {
  skip_on_os("windows")

  local_reproducible_output(crayon = TRUE, unicode = TRUE)
  expect_snapshot(tidyverse_logo())

  local_reproducible_output(crayon = FALSE, unicode = FALSE)
  expect_snapshot(tidyverse_logo())
})
