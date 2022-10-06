.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  tidyverse_attach()

  if (!"package:conflicted" %in% search() && !is_loading_for_tests()) {
    x <- tidyverse_conflicts()
    msg(tidyverse_conflict_message(x), startup = TRUE)
  }

}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
