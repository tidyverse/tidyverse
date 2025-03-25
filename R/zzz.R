.onAttach <- function(...) {
  if (is_loading_for_tests()) {
    return(invisible())
  }

  attached <- tidyverse_attach()
  inform_startup(tidyverse_attach_message(attached))

  if (is_attached("conflicted")) {
    return(invisible())
  }

  conflicts <- tidyverse_conflicts()
  inform_startup(tidyverse_conflict_message(conflicts))
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

is_loading_for_tests <- function() {
  !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "tidyverse")
}
