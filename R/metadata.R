#' coldesc
#'
#' @param df data frame
#'
#' @return a descriptive table of the columns
#' @export
#'
#' @importFrom dplyr tibble
coldesc <- function(df, view = FALSE){

  make_range <- function(x) {
    if (inherits(x, c("numeric", "integer"))) {
      paste0(signif(range(x, na.rm=TRUE), digits = 4), collapse = " - ")

    } else if (inherits(x, c("factor", "character"))) {
      paste(length(unique(x)), "levels")

    } else {
      NA
    }
  }

  meta <- dplyr::tibble(Column_names=names(df),
         # Datatype = purrr::map_chr(df, typeof),
         Dataclass = purrr::map_chr(df, ~ toString(class(.x))),
         Range = purrr::map_chr(df, make_range),
         Perc_complete = round(colSums(!is.na(df))/nrow(df)*100, 1))

  if (view) {
    View(meta, "meta")
  } else {
    print(meta, n = 40)
  }

  invisible(meta)

}
