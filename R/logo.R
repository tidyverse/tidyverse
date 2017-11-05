
#' The tidyverse logo, using ASCII or Unicode characters
#'
#' Use [crayon::strip_style()] to get rid of the colors.
#'
#' @param unicode Whether to use Unicode symbols. Default is `TRUE`
#'   on UTF-8 platforms.
#'
#' @md
#' @export
#' @examples
#' tidyverse_logo()

tidyverse_logo <- function(unicode = l10n_info()$`UTF-8`) {
  logo <- c(
    "0 __  _    __   1    2           3  4 ",
    " / /_(_)__/ /_ ___  _____ _______ ___ ",
    "/ __/ / _  / // / |/ / -_) __(_-</ -_)",
    "\\__/_/\\_,_/\\_, /|___/\\__/_/ /___/\\__/ ",
    "     5  6 /___/      7      8       9 ")

  hexa <- c("*", ".", "o", "*", ".", "*", ".", "o", ".", "*")
  if (unicode) hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]

  cols <- c("red", "yellow", "green", "magenta", "cyan",
            "yellow", "green", "white", "magenta", "cyan")

  col_hexa <- map2(hexa, cols, ~ crayon::make_style(.y)(.x))

  for (i in 0:9) {
    pat <- paste0("\\b", i, "\\b")
    logo <- sub(pat, col_hexa[[i + 1]], logo)
  }

  structure(crayon::blue(logo), class = "tidyverse_logo")
}

#' @export

print.tidyverse_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}
