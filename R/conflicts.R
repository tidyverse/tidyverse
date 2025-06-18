#' Conflicts between the tidyverse and other packages
#'
#' This function lists all the conflicts between packages in the tidyverse
#' and other packages that you have loaded.
#'
#' There are four conflicts that are deliberately ignored: \code{intersect},
#' \code{union}, \code{setequal}, and \code{setdiff} from dplyr. These functions
#' make the base equivalents generic, so shouldn't negatively affect any
#' existing code.
#'
#' @export
#' @param only Set this to a character vector to restrict to conflicts only
#'   with these packages.
#' @examples
#' tidyverse_conflicts()
tidyverse_conflicts <- function(only = NULL) {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- purrr::set_names(envs)

  if (!is.null(only)) {
    only <- union(only, core)
    envs <- envs[names(envs) %in% paste0("package:", only)]
  }

  objs <- invert(lapply(envs, ls_env))

  conflicts <- purrr::keep(objs, \(obj) length(obj) > 1)

  tidy_names <- paste0("package:", tidyverse_packages())
  conflicts <- purrr::keep(conflicts, function(pkg) any(pkg %in% tidy_names))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  class(conflict_funs) <- "tidyverse_conflicts"
  conflict_funs
}

tidyverse_conflict_message <- function(x) {
  header <- cli::rule(
    left = cli::style_bold("Conflicts"),
    right = "tidyverse_conflicts()"
  )

  pkgs <- x |> purrr::map(\(x) gsub("^package:", "", x))
  others <- pkgs |> purrr::map(`[`, -1)
  other_calls <- purrr::map2_chr(
    others,
    names(others),
    \(pkg, fun) paste0(cli::col_blue(pkg), "::", fun, "()", collapse = ", ")
  )

  winner <- pkgs |> purrr::map_chr(1)
  funs <- format(paste0(
    cli::col_blue(winner),
    "::",
    cli::col_green(paste0(names(x), "()"))
  ))
  bullets <- paste0(
    cli::col_red(cli::symbol$cross),
    " ",
    funs,
    " masks ",
    other_calls,
    collapse = "\n"
  )

  conflicted <- paste0(
    cli::col_cyan(cli::symbol$info),
    " ",
    "Use the ",
    cli::format_inline(
      "{.href [conflicted package](http://conflicted.r-lib.org/)}"
    ),
    " to force all conflicts to become errors"
  )

  paste0(
    header,
    "\n",
    bullets,
    "\n",
    conflicted
  )
}

#' @export
print.tidyverse_conflicts <- function(x, ..., startup = FALSE) {
  cli::cat_line(tidyverse_conflict_message(x))
  invisible(x)
}

#' @importFrom magrittr %>%
confirm_conflict <- function(packages, name) {
  # Only look at functions
  objs <- packages |>
    purrr::map(\(pkg) get(name, pos = pkg)) |>
    purrr::keep(is.function)

  if (length(objs) <= 1) {
    return()
  }

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1) {
    return()
  }

  packages
}

ls_env <- function(env) {
  x <- ls(pos = env)

  # intersect, setdiff, setequal, union come from generics
  if (env %in% c("package:dplyr", "package:lubridate")) {
    x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
  }

  if (env == "package:lubridate") {
    x <- setdiff(
      x,
      c(
        "as.difftime", # lubridate makes into an S4 generic
        "date" # matches base behaviour
      )
    )
  }

  x
}
