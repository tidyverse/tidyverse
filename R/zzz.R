.onAttach <- function(...) {
  attached <- tidyverse_attach()
  if (!is_loading_for_tests()) {
    inform_startup(tidyverse_attach_message(attached))
  }

  if (!is_attached("conflicted") && !is_loading_for_tests()) {
    conflicts <- tidyverse_conflicts()
    inform_startup(tidyverse_conflict_message(conflicts))
  }
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

is_loading_for_tests <- function() {
  !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "tidyverse")
}
