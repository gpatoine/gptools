#created: 2020-11-11
#updated: 2020-11-11
# author: Guillaume Patoine <guillaume.patoine@idiv.de>
#purpose: convenience functions to write and read timestamped files.
# Only for RDS format


# TODO rename files from old to new tmst scheme

#' Timestamp
#'
#' Squishes, especially useful for file names
#'
#' @param time logical Should time (HMS) also be included?
#'
#' @return character squishes timestamp
#' @export
tmst <- function(ext = NULL, time = T, prefix = "_c") {

  if (time) {
    paste0(prefix, format(Sys.time(), "%Y-%m-%d_%H%M%S"), ext)
  } else {
    paste0(prefix, format(Sys.time(), "%Y-%m-%d"), ext)
  }
}

# deprec
# tmst <- function(time = T) {
#   if (time) {
#     format(Sys.time(), "%Y%m%d%H%M%S")
#   } else {
#     format(Sys.time(), "%Y%m%d")
#   }
# }


#' Last timestamped
#'
#' Read last timestamped (RDS) file
#' Default is to load file, but can return only name
#'
#' @param fold folder path
#' @param pattern regex pattern passed to list.files
#' @param load logical wanna load to file or just get it's name. use FALSE if the file is not rds format
#' @param prev int previous version before last
#'
#' @return R object read from RDS file
#' @export
last_tmst <- function(fold, pattern = "", load = TRUE, prev = 0) {
  files <- list.files(fold, pattern = pattern, full.names = TRUE)
  file <- files %>% sort(TRUE) %>% .[1 + prev]

  if (load & tools::file_ext(file) == "rds") {
    message("Reading ", basename(file))
    readRDS(file)

  }  else file

}



#' List timestamped
#'
#' List timestamped files fitting a pattern with parsed date.
#'
#' @param fold path
#' @param pattern pattern passed to list.files
#'
#' @return tibble
#' @export
list_tmst <- function(fold, pattern = ".", recursive = FALSE) {

  files <- list.files(fold, pattern = pattern, recursive = recursive)

  ind14 <- files %>% str_detect("\\d{14}")

  stamps14 <- lubridate::ymd_hms(stringr::str_extract(files[ind14], "\\d{14}"))

  stamps_sep <- lubridate::ymd_hms(stringr::str_extract(files[!ind14], "\\d{4}-\\d{2}-\\d{2}_\\d{6}"))

  # TODO add without hms

  stamps_all <- lubridate::as_datetime(NA)
  stamps_all[ind14] <- stamps14
  stamps_all[!ind14] <- stamps_sep

  dplyr::arrange(tibble(files, stamps_all), dplyr::desc(stamps_all)) %>% print(n = 30)

}

# examples
# list_tmst("~/Documents/Projects_local/globcmic/data_mod")
# list_tmst("~/Documents/Projects_local/globcmic/data_mod", "04-cmic_extracts_grouped_sf_c")
# list_tmst("~/Documents/Projects_local/globcmic/data_mod", "extr", recursive = T)
