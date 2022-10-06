test_that("if no packages, shows nothing", {
  expect_snapshot(cat(tidyverse_attach_message(character())))
})

test_that("message lists all core tidyverse packages", {
  mockr::local_mock(package_version = function(x) "1.0.0")
  expect_snapshot(cat(tidyverse_attach_message(core)))
})
