bullets <- function(...) {
  message(paste0(" * ", ..., collapse = "\n"))
}

startup_message <- function(...) {
  if (isTRUE(getOption("tidyverse.quiet")))
    return()

  packageStartupMessage(...)
}

rule <- function(..., pad = "-", startup = FALSE) {
  if (nargs() == 0) {
    title <- ""
  } else {
    title <- paste0(...)
  }
  width <- min(getOption("width") - nchar(title) - 1, 68)

  text <- cli::rule(left = title, width = width, line_col = "black")

  if (startup) {
    startup_message(text)
  } else {
    message(text)
  }
}

#' List all packages in the tidyverse
#'
#' @param include_self Include tidyverse in the list?
#' @export
#' @examples
#' tidyverse_packages()
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


style_grey <- function(level, ...) {
  crayon::style(
    paste0(...),
    crayon::make_style(grDevices::grey(level), grey = TRUE)
  )
}
