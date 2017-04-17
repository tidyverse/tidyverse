.onAttach <- function(...) {
  tidyverse_attach()
  tidyverse_conflicts()
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
