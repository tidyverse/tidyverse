#' @keywords internal
"_PACKAGE"

# Suppress R CMD check note
# Namespace in Imports field not imported from: PKG
#   All declared Imports should be used.
ignore_unused_imports <- function() {
  broom::tidy
  conflicted::conflict_prefer
  dbplyr::db_collect
  dtplyr::lazy_dt
  googledrive::drive_find
  googlesheets4::read_sheet
  haven::read_sas
  hms::hms
  httr::GET
  jsonlite::fromJSON
  modelr::add_predictions
  pillar::pillar
  ragg::agg_png
  readxl::read_excel
  reprex::reprex
  rlang::qq_show
  rvest::html_node
  xml2::read_xml
}

release_bullets <- function() {
  c(
    '`usethis::use_latest_dependencies(TRUE, "CRAN")`',
    '`tidyverse_dependency_dissuade()`'
  )
}
