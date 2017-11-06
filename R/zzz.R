.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  crayon::num_colors(TRUE)
  tidyverse_attach()

  x <- tidyverse_conflicts()
  msg(tidyverse_conflict_message(x), startup = TRUE)
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
