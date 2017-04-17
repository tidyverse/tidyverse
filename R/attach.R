core <- c("ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr")

tidyverse_attach <- function() {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  packageStartupMessage(paste0("Loading tidyverse: ", needed, collapse = "\n"))
  suppressPackageStartupMessages(
    lapply(needed, library, character.only = TRUE, warn.conflicts = FALSE)
  )
}

