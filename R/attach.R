core <- c("ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr", "stringr", "forcats")

tidyverse_attach <- function() {
  msg(
    cli::rule(
      left = crayon::bold("Attaching packages"),
      right = paste0("tidyverse ", package_version("tidyverse"))
    ),
    startup = TRUE
  )
  versions <- vapply(core, package_version, character(1))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(core)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  col1 <- 1:floor(length(packages)/2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(core, library, character.only = TRUE, warn.conflicts = FALSE)
  )

  invisible()
}

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- crayon::red(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}
