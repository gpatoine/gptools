

#check nonnum values
#' which.nonnum
#'
#' @param x
#'
#' @return
#' @export
which.nonnum <- function(x) {
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  which(badNum & !is.na(x))
}


#' nonumdf
#'
#' @param df
#' @param cols
#'
#' @return
#' @export
nonumdf <- function(df, cols = seq_along(df)){
  #df = fv1
  #

  nonumlist <- lapply(df %>% select(New_ID, all_of(cols)), which.nonnum)

  nonumdf <- NULL

  for (ielem in seq_along(nonumlist)) {
    #ielem <- 3

    if (length(nonumlist[[ielem]]) == 0) {
      next
    }

    nonumval <- tibble(row = nonumlist[[ielem]],
                       column = names(nonumlist)[ielem],
                       New_ID = df %>% select(New_ID) %>% slice(row) %>% unlist,
                       value = df %>% select(names(nonumlist)[ielem]) %>% slice(row) %>% unlist
    )

    nonumdf <- bind_rows(nonumdf, nonumval)

  }

  nonumdf

}



#' repl_dict
#'
#' dictionary of terms to replace
#'
#' @param x
#' @param dict
#'
#' @return
#' @export
repl_dict <- function (x, dict) {

  for(idict in seq_along(dict)) {
    x[x == names(dict[idict])] <- dict[idict]
  }

  x
}


check_modif <- function (col1, col2) {
  tibble(col1, col2) %>%
    filter(!map2_lgl(col1, col2, identical))
}


#' dputran
#'
#' dput with integer ranges
#'
#' @param x
#'
#' @return
#' @export
gp_dputran <- function(x) {
  sx <- sort(x) %>% unique
  diff <- sx[-1] - sx[-length(sx)]

  same <- diff == 1

  string <- sx[1] %>% as.character

  pos <- 2
  len <- length(sx)
  follow <- 0

  while (pos <= len){

    if (same[pos-1]) {

      follow <- follow + 1

    } else {

      if (follow > 0) {
        string <- paste0(string, ":", sx[pos-1], sep = "")
      }

      follow <- 0
      string <- paste0(string, ",", sx[pos])

    }

    pos <- pos + 1

  }

  if (follow > 0) {
    string <- paste0(string, ":", sx[pos-1], sep = "")
  }

  if (str_detect(string, ",")) {
    string <- paste0("c(", string, ")")
  }

  cat(string)
  invisible(string)

}



#' File opened
#'
#' Check if file is available for writing
#'
#' @param path
#'
#' @return logical
#' @export
gp_file_opened <- function(path) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(path,
               open = "w"),
          silent = TRUE
      )
    )
  )
}



#' Job Info
#'
#' Retrieves job info. Doesn't do much for now
#'
#' @return list
#' @export
gp_jobinfo <- function() {
  .rs.invokeRpc("get_jobs")
}


