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
saveme <- function(x, file, comment = NULL, FUN = saveRDS, ...) { #ovr_today = FALSE,

  obj_name <- deparse(match.call()$x)

  # do the thing to save metadata
  record_meta(x, file, comment, obj_name)

  FUN(x, file, ...)

  message("Size: ", fs::file_size(file))

  # TODO check if previous file exists and overwrite from today
  # actually not sure how to do that

  # if (ovr_today & !is.na(file)) {
  #
  #   file.remove()
  #
  # }


}



#' GGsave metadata
#'
#' Source is empty if the function was called from an unsaved script.
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

  ft_dir <- if ("ft_dir" %in% ls(envir = .GlobalEnv)) {
    get("ft_dir", envir = .GlobalEnv)
  } else {
    ""
  }

  ft_path <- here(paste0(ft_dir, "project_file_tracking.csv"))

  if (!file.exists(ft_path)) {
    stop(paste0(ft_path, " not found. Create it with create_project_file_tracking()"))
  }

  # TODO understand why rstudioapi works from local job on local computer but not on remote desktop ??? R version?
  rstu <- rstudioapi::isAvailable()

  if (is.null(obj_name)) obj_name <- deparse(match.call()$x)

  # adjust comment
  if (is.null(comment)) {
    if (rstu) {
      comment <- rstudioapi::showPrompt("File metadata", "Comment:")

    } else if (interactive()) {
      cat("Saving file metadata. Comment: ")
      comment <- readLines("stdin", 1)

    } else {
      comment <- "non-interactive"

    }

    if (is.null(comment)) comment <- "no comment" #if e.g. cancel prompt

  }

  dirname_rel = if (rstu) {
    if (dirname(file) == rstudioapi::getActiveProject()) {
      "project_root"
      } else {
        str_remove(dirname(file), paste0("^", rstudioapi::getActiveProject(), "/"))
        }
    } else {
      "unknown"
    }


  tib <- tibble(
    date = Sys.Date(),
    time = format(Sys.time(), "%H:%M:%S"),
    basename = basename(file),
    dirname_rel = dirname_rel,
    comment = comment,
    source = if (rstu) rstudioapi::getActiveDocumentContext()$path else "unknown",
    machine = Sys.info()["nodename"],
    object_name = obj_name,
    object_class = toString(class(x)),
    dim = if(is.data.frame(x)) toString(dim(x)) else "NA",
    full_path = path.expand(file)

  )

  # write to csv
  write_csv(tib, ft_path, append = TRUE)

}

# saveme(cmic, here("data_mod/test_cmic_saveit.rds"), saveRDS)


#' create_project_file_tracking
#'
#' Create tracking file
#'
#' @param dir chr In which folder should the file be created. Defaults to project folder.
#' @param add_profile logical Should a note be done in .Rprofile for non-default dir
#'
#' @return NULL
#' @export
create_project_file_tracking <- function(dir = NULL, add_profile = FALSE) {

  ft_path <- here(paste0(dir, "project_file_tracking.csv"))

  if (file.exists(ft_path)) {
    stop(paste0(ft_path, " exists already. Delete it first to create a new one."))
  }

  tib <- tibble(
    date = NA,
    time = NA,
    basename = NA,
    dirname_rel = NA,
    comment = NA,
    source = NA,
    machine = NA,
    object_name = NA,
    object_class = NA,
    dim = NA,
    full_path = NA
  ) %>% slice(0)

  write_csv(tib, ft_path)

  if (add_profile & !is.null(dir)) {

    if (file.exists(here(".Rprofile"))) {
      write(dir, file = here(".Rprofile"), append=TRUE)
    } else {
      warning("No .Rprofile found.")
    }
  }

  insvisible(NULL)

}
