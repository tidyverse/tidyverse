core <- c("ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr", "stringr")

tidyverse_attach <- function() {
  versions <- vapply(core, function(x) as.character(utils::packageVersion(x)), character(1))
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
    OS = os(),
    GUI = ui,
    Locale = Sys.getlocale("LC_COLLATE"),
    TZ = Sys.timezone()
  )
}

os <- function() {
  x <- utils::sessionInfo()$running

  # Regexps to clean up long windows strings generated at
  # https://github.com/wch/r-source/blob/af7f52f70101960861e5d995d3a4bec010bc89e6/src/library/utils/src/windows/util.c

  x <- gsub("Service Pack", "SP", x)
  x <- gsub(" (build \\d+)", "", x)

  x
}
