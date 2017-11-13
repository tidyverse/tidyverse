msg <- function(..., startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("tidyverse.quiet"))) {
      packageStartupMessage(text_col(...))
    }
  } else {
    message(text_col(...))
  }
}

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)

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
