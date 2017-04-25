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
#' @examples
#' tidyverse_conflicts()
tidyverse_conflicts <- function() {
  tidy_names <- paste0("package:", tidyverse_packages())

  tidy_envs <- intersect(tidy_names, search())
  names(tidy_envs) <- tidy_envs

  if (length(tidy_envs) == 0)
    return(invisible())

  misc_envs <- setdiff(search(), tidy_envs)
  names(misc_envs) <- misc_envs

  tidy_funs <- invert(lapply(tidy_envs, ls_env))
  misc_funs <- invert(lapply(misc_envs, ls_env))

  conflicts <- intersect(names(tidy_funs), names(misc_funs))

  conflict_funs <- purrr::map2(tidy_funs[conflicts], misc_funs[conflicts], c)
  conflict_funs <- purrr::map2(purrr::set_names(names(conflict_funs)), conflict_funs,
    confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  rule("Conflicts", startup = TRUE)
  funs <- format(paste0(names(conflict_funs), "(), "))

  pkgs <- conflict_funs %>% purrr::map(~ gsub("^package:", "", .))
  winner <- pkgs %>% purrr::map_chr(1)
  others <- pkgs %>% purrr::map(`[`, -1)
  other_calls <- purrr::map2_chr(
    others, names(others),
    ~ paste0(.x, "::", .y, "()", collapse = ", ")
  )

  msg <- paste0("* ", funs, " from ", winner, ", masks ", other_calls, collapse = "\n")
  startup_message(msg)
}

#' @importFrom magrittr %>%
confirm_conflict <- function(name, packages) {
  # Only look at functions
  objs <- packages %>%
    purrr::map(~ get(name, pos = .)) %>%
    purrr::keep(is.function)

  if (length(objs) <= 1)
    return()

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1)
    return()

  packages
}

ls_env <- function(env) {
  x <- ls(pos = env)
  if (identical(env, "package:dplyr")) {
    x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
  }
  x
}
