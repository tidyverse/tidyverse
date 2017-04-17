core <- c("ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr", "stringr")

tidyverse_attach <- function() {
  versions <- vapply(core, function(x) as.character(packageVersion(x)), character(1))
  packages <- paste0("+ ", format(core), " ", format(versions))

  info <- platform_info()
  info <- paste0(format(paste0(names(info)), justify = "right"), ": ", info)

  n <- max(length(packages), length(info))
  info <- c(info, rep("", n - length(info)))

  info <- paste0(packages, "      ", info, collapse = "\n")

  packageStartupMessage(info)
  suppressPackageStartupMessages(
    lapply(core, library, character.only = TRUE, warn.conflicts = FALSE)
  )

  invisible()
}



platform_info <- function() {
  if (rstudioapi::isAvailable()) {
    ver <- rstudioapi::getVersion()
    ui <- paste0("RStudio ", ver, "")
  } else {
    ui <- .Platform$GUI
  }

  ver <- R.version

  c(
    Date = format(Sys.Date()),
    R = paste0(ver$major, ".", ver$minor),
    GUI = ui,
    Locale = Sys.getlocale("LC_COLLATE"),
    TZ = Sys.timezone()
  )

}
