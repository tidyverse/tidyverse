bullets <- function(...) {
  message(paste0(" * ", ..., collapse = "\n"))
}

rule <- function(..., pad = "-") {
  if (nargs() == 0) {
    title <- ""
  } else {
    title <- paste0(...)
  }
  width <- getOption("width") - nchar(title) - 1
  message(title, " ", paste(rep(pad, width, collapse = "")))
}

tidyverse_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("tidyverse")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)

  if (include_self) {
    parsed <- c(parsed, "tidyverse")
  }

  parsed
}

invert <- function(x) {
  if (length(x) == 0) return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}
