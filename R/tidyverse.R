#' @keywords internal
"_PACKAGE"

# Suppress R CMD check note
#' @importFrom broom tidy
#' @importFrom dbplyr db_collect
#' @importFrom dtplyr lazy_dt
#' @importFrom forcats fct_c
#' @importFrom ggplot2 ggplot
#' @importFrom googledrive drive_find
#' @importFrom googlesheets4 read_sheet
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


release_bullets <- function() {
  c(
    '`usethis::use_latest_dependencies(TRUE, "CRAN")`',
    '`tidyverse_dependency_dissuade()`'
  )
}
