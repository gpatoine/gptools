# Data versioning
# TODO default_fold should point to archd folder in the project folder with
# mirrored folder structure
# Keeps most recent version (MRV) in folder, all other version timestamped in
# archd


#' ad_init
#'
#' Initialize file: Use on an existing file to track and create to repository folder.
#'
#' @param file The file of interest
#' @param fold_path The folder location for the data archive
#'
#' @return Nothing
#' @export
ad_init <- function(file, fold_path = default_fold(file)){
  #file <- zefile
  #fold_path <- folpat

  # 1-directory
  # does repo exist
  if(dir.exists(fold_path)){
    stop("directory exists already")
  } else {
    dir.create(fold_path, recursive = T)
    message("The ArchD directory was created: ", fold_path)
  }

  # 2-save file
  # create timestamped version
  ts_path <- mk_timestamp(file.path(fold_path, basename(file)))
  if(file.copy(file, ts_path)){
    message("Success: File ArchDed")
  } else {
    stop("could not archive file.")
  }

  # 3-create + fill log file

  log_tab <- create_log(file, ts_path)

  saveRDS(log_tab, file.path(fold_path, "log_archD.rds"))

  #CSV only for consultation, not for manual edits
  list_cols_to_csv(df = log_tab, file_path = file.path(fold_path, "log_archD.csv"))

}



#' ad_archive
#'
#' Save a timestamped file of the MRV, useful for manual edits
#'
#' @param file The file of interest
#' @param fold_path The folder location for the data archive
#'
#' @return nothing
#' @export
ad_archive <- function(file, fold_path = default_fold(file)){
  # check repo
  if(!dir.exists(fold_path)){
    stop("No archD repo. Use ad_init() to create one.")
  }

  if(suppressMessages(ad_to_date(file, fold_path))){
    message("No need to archD. Already up to date!")
  } else {
    # create timestamped version
    ts_path <- mk_timestamp(file.path(fold_path, basename(file)))
    if(file.copy(file, ts_path)){
      message("Success: File archDed")

      update_log(file, fold_path, method = "ad_archive")

    } else {
      stop("could not archive file.")
    }

  }

}



#' ad_save
#'
#' Save an R object to file
#'
#' @param x object
#' @param file The file of interest
#' @param fold_path The folder location for the data archive
#'
#' @return nothing
#' @export
ad_save <- function(x, file, fold_path = default_fold(file)){
  # x <- df
  # check repo
  if(!dir.exists(fold_path)){
    stop("No ArchD repo. Use ad_init() to create one.")
  } else {

    # create timestamped version
    ts_name <- mk_timestamp(file.path(fold_path, basename(file)))
    rio::export(x, ts_name)

    # overwrite MRV
    rio::export(x, file)

    update_log(file, fold_path, method = "ad_save")

  }
}



#' ad_to_date
#'
#' Check if archd is up to date
#' MRV: most recent version in the "working directory"
#' LSV: last saved version in the archd repo
#'
#' @param file The file of interest
#' @param fold_path The folder location for the data archive
#'
#' @return nothing
#' @export
ad_to_date <- function(file, fold_path = default_fold(file)){
  # find last saved version

  lsv <- ad_lsv(file, fold_path)

  to_date <- tools::md5sum(lsv) == tools::md5sum(file)

  if(to_date){
    message("MRV and LSV correspond. ArchD is up to date.")
  } else {
    message("The MRV and LSV appear different. Update ArchD with ad_archive().")
  }

  isTRUE(to_date)
}



#' ad_lsv
#'
#' returns version name
#'
#' @param file file
#' @param fold_path fold
#' @param look_back positive integer. zero is the lsv, higher numbers for earlier versions
#'
#' @return ?
#' @export
ad_lsv <- function(file, fold_path = default_fold(file), look_back = 0){
  # look_back is 0 for lsv, and increases for earlier version. no negative numbers

  # find most recent version
  saved_files <- list.files(fold_path, full.names = TRUE)

  rm_log <- saved_files[basename(saved_files) != "log_archD.csv" & basename(saved_files) != "log_archD.rds"]

  if ((look_back+1) > length(rm_log)) {
    warning("Not enough versions for given look_back value. Returning the earliest version available.")
    sort(rm_log)[1]
  } else {
    sort(rm_log)[length(rm_log) - look_back]

  }

}



# convenience functions ---------------------------------------------------

#' default_fold
#'
#' suggested folder to create the archive. Default option is to have an "archd" repo in the project folder with a mirrored folder structure.
#'
#' @param file The file of interest
#'
#' @return default archd repo path
#' @export
default_fold <- function(file) {
  file.path(here::here(),
            paste0("archd",
                   tools::file_path_sans_ext(str_remove(file, paste0(here::here()))),
                   "_",
                   tools::file_ext(file)
            )
  )
}



#' mk_timestamp
#'
#' adds a timestamp YYYYMMDD_HHMMSS to the file path
#'
#' @param file The file of interest
#'
#' @return file path with timestamp
#' @export
mk_timestamp <- function(file) {
  file.path(dirname(file),
            paste0(basename(tools::file_path_sans_ext(file)),
                   "-_-",
                   # datetime
                   gsubfn::gsubfn(".", list(":" = "", "-" = "", " " = "_"),
                          as.character(Sys.time())),
                   ".",
                   tools::file_ext(file)
            )
  )
}



#' same_file
#'
#' @param file1 chr path
#' @param file2 chr path
#'
#' @return logical
#' @export
same_file <- function(file1, file2){
  tools::md5sum(file1) == tools::md5sum(file2)
}



#' create_log
#'
#' @param file chr path
#' @param ts_path chr path
#'
#' @return ?
#' @export
create_log <- function(file, ts_path){

  comment <- winDialogString("Comment:", "")
  if(is.null(comment)) comment <- "cancelled"

  dplyr::tibble(filename = ts_path,
                previous = file,
                change = list("init"),
                method = "ad_init",
                comment = comment
  )
}



#' update_log
#'
#' @param file file path
#' @param fold_path fold
#' @param method chr
#'
#' @return ?
#' @export
update_log <- function(file, fold_path, method = NA){

  # compare dfs

  df1_path <- ad_lsv(file, fold_path, look_back = 1)
  df2_path <- ad_lsv(file, fold_path)

  # TODO maybe import as text if csv/excel format

  df1 <- rio::import(df1_path) #, col_types = "text")
  df2 <- rio::import(df2_path) #, col_types = "text")

  df_diff <- comp_dfs(df1, df2)

  comment <- winDialogString("Comment:", "")
  if(is.null(comment)) comment <- "cancelled"

  current_log <- dplyr::tibble(filename = df2_path,
                               previous = df1_path,
                               change = list(df_diff),
                               method = method,
                               comment = comment
  )

  #load log_file
  past_log <- readRDS(file.path(fold_path, "log_archD.rds"))
  #full_log$change <- as.list(full_log$change)

  updated_log <- dplyr::bind_rows(past_log, current_log)

  saveRDS(updated_log, file.path(fold_path, "log_archD.rds"))

  #only with current_log since using append in write_
  list_cols_to_csv(df = current_log, file_path = file.path(fold_path, "log_archD.csv"),
                   append = TRUE)

}


#' comp_dfs
#'
#' @param df1 data.frame
#' @param df2 data.frame
#'
#' @return character vector of changes
#' @export
comp_dfs <- function(df1, df2){

  results0 <- all.equal(df1, df2)

  if (isTRUE(results0)) {
    "No change"

  } else{
    stringr::str_remove_all(results0, "“|”|Component |string ")

  }
}



#' list_cols_to_csv
#'
#' @param df data.frame
#' @param file_path chr
#' @param ... passed to write_excel_csv
#'
#' @return ?
#' @export
list_cols_to_csv <- function(df, file_path, ...) {
  #
  set_lists_to_chars <- function(x) {
    if(class(x) == 'list') { y <- map_chr(x, toString) } else { y <- x  }
    return(y) }
  new_frame <- as_tibble(map(df, set_lists_to_chars))
  readr::write_excel_csv(new_frame, file_path, ...)
}

#TODO implement better compare, see notes
# compare_dfs2 <- function(df1, df2){
#   # check missing columns
#   # check missing rows
#   # subset matching dfs (cols + IDs)
#   # sort by New_ID
#   # return mismatches per column
# }

#tools::md5sum(list.files(here("archd/archd_5-soil_3_12_xlsx"), full.names = TRUE))

# Use cases ----------------------------------------------------------------

# library(here)
#
# zefile <- here("archd/5-soil_3_12.xlsx")
#
# ad_init(zefile)
# ad_archive(zefile)
# ad_to_date(zefile)
#
# df <- rio::import(here("archd/5-soil_3_12.xlsx"))
#
# file1 <- "C:/Users/gp63dyte/Documents/Projects_local/Misc/archd/archd_5-soil_3_12_xlsx/5-soil_3_12-_-20200721_105749.xlsx"
# file2 <- "C:/Users/gp63dyte/Documents/Projects_local/Misc/archd/archd_5-soil_3_12_xlsx/5-soil_3_12-_-20200721_112422.xlsx"
#
# df1 <- rio::import(file1)
# df2 <- rio::import(file2)
#
# all.equal(df1, df2)
# identical(df1, df2)
# same_file(file1, file2)
#
#
#

# Better compare_dfs functions --------------------------------------------

# https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in
# https://cran.r-project.org/web/packages/arsenal/vignettes/comparedf.html
# https://www.r-bloggers.com/comparing-dataframes-in-r-using-comparedf/
# https://cran.r-project.org/web/packages/dataCompareR/dataCompareR.pdf
# https://bookdown.org/Maxine/r4ds/comparing-two-data-frames-tibbles.html
#
# check missing columns
# check missing rows
# subset matching dfs (cols + IDs)
# sort by New_ID
# return mismatches per column

# save list to CSV
# https://stackoverflow.com/questions/48024266/save-a-data-frame-with-list-columns-as-csv-file
# https://community.rstudio.com/t/what-is-best-method-to-save-and-read-data-frames-with-list-column/1718/2
