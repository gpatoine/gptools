#created: 2020-11-14
#updated: 2020-11-14
# author: Guillaume Patoine <guillaume.patoine@idiv.de>
#purpose: Save with added metadata. Mostly useful for data, but potentially relevant for figures and such

# NOTE: any reason to keep these separated? Why not just one function?


#' Save metadata
#'
#' Wrapper function that saves an object using FUN and related metadata.
#' Supported functions include saveRDS(), write_csv() and write_excel_csv().
#'
#' Doesn't currently work with ggsave.
#'
#' @param x Object to save
#' @param file character Path to save file
#' @param comment character
#' @param FUN function used for saving. Default is saveRDS.
#' @param ... additional arguments passed to FUN
#'
#' @return
#' @export
saveme <- function(x, file, comment = NULL, FUN = saveRDS, ...) {

  obj_name <- deparse(match.call()$x)

  # do the thing to save metadata
  record_meta(x, file, comment, obj_name)

  # TODO add functionality for ggsave

  FUN(x, file, ...)

}



#' Record metadata
#'
#' Writes metadata to CSV file located in archd project folder. Tracks changes.
#'
#' @param x Object to save
#' @param file character Path to save file
#' @param comment character
#' @param obj_name only used if called internally
#'
#' @return
#' @export
record_meta <- function(x, file, comment = NULL, obj_name = NULL) {

  if (is.null(obj_name)) obj_name <- deparse(match.call()$x)

  # adjust comment
  if (is.null(comment)) {
    if (interactive()) {
      comment <- rstudioapi::showPrompt("File metadata", "Comment:")

    } else {
      comment <- "non-interactive session"

    }
  }


  tib <- tibble(
    date = Sys.Date(),
    time = format(Sys.time(), "%H:%M:%S"),
    file = file,
    source = rstudioapi::getActiveDocumentContext()$path,
    object_name = obj_name,
    object_class = toString(class(x)),
    description = comment
  )

  # write to csv
  write_csv(tib, here("archd/project_file_tracking.csv"), append = TRUE)

}


# saveme(cmic, here("data_mod/test_cmic_saveit.rds"), saveRDS)
