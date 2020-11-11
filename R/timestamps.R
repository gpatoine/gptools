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
#'
#' @examples
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
#'
#' @examples
last_tmst <- function(fold, pattern) {
  files <- list.files(fold, pattern = pattern, full.names = TRUE)
  file <- files %>% sort(TRUE) %>% .[1]
  message("Reading ", basename(file))
  readRDS(file)
}


