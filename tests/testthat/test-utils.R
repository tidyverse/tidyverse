test_that("tidyverse packages returns character vector of package names", {
  out <- tidyverse_packages()
  expect_type(out, "character")
  expect_true("ggplot2" %in% out)
})
