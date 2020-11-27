#created: 2020-11-11
#updated: 2020-11-11
# author: Guillaume Patoine <guillaume.patoine@idiv.de>
#purpose: convenience functions to write and read timestamped files.
# Only for RDS format



#' Timestamp
#'
#' Squishes, especially useful for file names
#'
#' @param time logical Should time (HMS) also be included?
#'
#' @return character squishes timestamp
#' @export
tmst <- function(time = T) {
  if (time) {
    format(Sys.time(), "%Y%m%d%H%M%S")
  } else {
    format(Sys.time(), "%Y%m%d")
  }
}



#' Read last timestamped RDS file
#'
#' @param fold
#' @param pattern
#'
#' @return R object read from RDS file
#' @export
last_tmst <- function(fold, pattern) {
  files <- list.files(fold, pattern = pattern, full.names = TRUE)
  file <- files %>% sort(TRUE) %>% .[1]
  message("Reading ", basename(file))
  readRDS(file)
}



#' List timestamped
#'
#' List timestamped files fitting a pattern with parsed date.
#'
#' @param fold path
#' @param pattern pattern passed to list.files
#'
#' @return
#' @export
list_tmst <- function(fold, pattern = ".", recursive = FALSE) {

  files <- list.files(fold, pattern = pattern, recursive = recursive)
  stamps <- lubridate::ymd_hms(stringr::str_extract(files, "\\d{14}"))
  dplyr::arrange(tibble(files, stamps), stamps) %>% print(n = 30)

}

# examples
# list_tmst("~/Documents/Projects_local/globcmic/data_mod")
# list_tmst("~/Documents/Projects_local/globcmic/data_mod", "04-cmic_extracts_grouped_sf_c")
# list_tmst("~/Documents/Projects_local/globcmic/data_mod", "extr", recursive = T)
