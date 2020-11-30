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
#' Use ggsaveme() for ggplot objects.
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



#' GGsave metadata
#'
#' @param filename character Path to save file
#' @param plot Object to save. Last plot if unspecified
#' @param comment character
#' @param ... additional arguments passed to ggsave
#'
#' @return
#' @export
ggsaveme <- function(filename, plot = NULL, comment = NULL,...) {

  if (is.null(plot)) {
    obj_name <- "NA"
    plot <- last_plot()
  } else {
    obj_name <- deparse(match.call()$plot)
  }

  record_meta(x = plot, filename, comment, obj_name)

  ggsave(filename, plot, ...)

}



#' Record metadata
#'
#' Writes metadata to CSV file located in archd project folder. Tracks changes.
#'
#' Only works from Rstudio
#'
#' @param x Object to save
#' @param file character Path to save file
#' @param comment character
#' @param obj_name only used if called internally
#'
#' @return
#' @export
record_meta <- function(x, file, comment = NULL, obj_name = NULL) {

  # TODO add options if not from Rstudio
  # rstudioapi::isAvailable()
  # or Sys.getenv("RSTUDIO", unset = NA)


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
    basename = basename(file),
    dirname_rel = str_remove(dirname(file), paste0("^", rstudioapi::getActiveProject(), "/")),
    comment = comment,
    source = rstudioapi::getActiveDocumentContext()$path,
    machine = Sys.info()["nodename"],
    object_name = obj_name,
    object_class = toString(class(x)),
    dim = if(is.data.frame(x)) toString(dim(x)) else "NA",
    full_path = path.expand(file)

  )

  # write to csv
  write_csv(tib, here("archd/project_file_tracking.csv"), append = TRUE)

}


# saveme(cmic, here("data_mod/test_cmic_saveit.rds"), saveRDS)
