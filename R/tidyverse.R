#' @keywords internal
"_PACKAGE"

# Suppress R CMD check note
#' @importFrom broom tidy
#' @importFrom dbplyr db_collect
#' @importFrom forcats fct_c
#' @importFrom ggplot2 ggplot
#' @importFrom haven read_sas
#' @importFrom hms hms
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd
#' @importFrom modelr add_predictions
#' @importFrom pillar pillar
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom reprex reprex
#' @importFrom rlang qq_show
#' @importFrom rvest html_node
#' @importFrom stringr str_c
#' @importFrom tidyr spread
#' @importFrom xml2 read_xml
NULL


release_questions <- function() {
  c(
    "Have you run `usethis::use_tidy_versions(TRUE)`?",
    "Have you tested with RStudio 1.0?"
  )
}
