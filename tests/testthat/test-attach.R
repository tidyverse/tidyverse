test_that("if no packages, shows nothing", {
  expect_snapshot(cat(tidyverse_attach_message(character())))
})

test_that("message lists all core tidyverse packages", {
  local_mocked_bindings(package_version_h = function(x) "1.0.0")
  expect_snapshot(cat(tidyverse_attach_message(core)))
})

test_that("highlights dev versions in red", {
  local_reproducible_output(crayon = TRUE)

  expect_snapshot({
    highlight_version(c("1.0.0", "1.0.0.9000", "0.9000.0.9000", "1.0.0-rc"))
  })
})
