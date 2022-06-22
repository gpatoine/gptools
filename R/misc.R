#created: 2022-04-28
#updated: 2022-04-28
# author: Guillaume Patoine <guillaume.patoine@idiv.de>
#purpose: description


#check nonnum values
#' which.nonnum
#'
#' @param x input
#'
#' @return logical
#' @export
which.nonnum <- function(x) {
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  which(badNum & !is.na(x))
}


#' nonumdf
#'
#' @param df data.frame
#' @param cols col number?
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
#' @param x object
#' @param dict list?
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



#' Scale2
#'
#' Same as scale but works with vectors, not matrices, and therefore with mutate
#'
#' @param x numeric vector to scale
#' @param na.rm logical
#'
#' @return
#' @export
gp_scale2 <- function(x, na.rm = FALSE) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
}




#' gp_ggaes
#'
#' ggplot possible aesthetics
#'
#' Lists all possible aesthetics arguments for a given geom
#'
#' @param geom character which geom?
#'
#' @return vector of argument names
#' @export
#'
#' @examples gp_ggaes("violin")
gp_ggaes <- function(geom) {
  ggplot2:::check_subclass(geom, "Geom")$aesthetics()
}


#' Write dataframe names
#'
#' Convenience function
#'
#' @param x anything that has names
#' @param sort alphabetical sorting
#'
#' @return x
#' @export
#' @examples wrnam(cars)
wrnam <- function(x, sort = FALSE) {

  if (sort) {
    writeLines(sort(names(x)))
  } else {
    writeLines(names(x))
  }

  invisible(x)

}


#' Make header
#'
#' Creates a section header
#' Need to position the cursor after function call and run with alt+enter for best results.
#'
#' @param text character
#' @param symbol single character
#' @param width numeric
#'
#' @return NULL
#' @export
#'
#' @examples mk_hdr("My Title")
mk_hdr <- function(text, symbol = "*", width = 60, symbol2 = "-") {

  deco <- paste0("# ", paste(rep(symbol, width), collapse = ""))

  pos <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start
  t_range <- rstudioapi::document_range(c(pos[1], 0), c(pos[1], pos[2]))

  text2 <- paste(deco, paste0("# ", stringr::str_pad(paste0(" ", text, " "), width, "both", symbol2)), deco, sep = "\n")

  rstudioapi::insertText(t_range, text2)

  NULL

}



#' Update header with today's date
#'
#' @return NULL
#' @export
up_date <- function() {

  up_text <- paste0("#updated: ", format(Sys.time(), "%Y-%m-%d"))
  cur_file <- rstudioapi::getActiveDocumentContext()$path

  hdr <- readr::read_lines(cur_file)[1:4]
  upd_line <- which(stringr::str_detect(hdr, "#updated"))

  if (length(upd_line) == 1) {

    t_range <- rstudioapi::document_range(c(upd_line, 0), c(upd_line, stringr::str_length(hdr[upd_line])+1))

    rstudioapi::insertText(t_range, up_text)

  } else {

    warning("Could not identify a single 'updated' header line")

  }

  invisible(NULL)

}


#' Paste NA
#'
#' paste that removes NAs
#'
#' @param ... character vectors
#' @param sep a character string to separate the terms.
#'
#' @return character vector
#' @export
#'
#' @examples
#' paste_na(c(1,2,3, NA), c('a', NA, NA, NA), c('a', 2, "NA", NA), sep = "--")
#' paste_na(c(1,NA,3, NA), c('a', 2, NA, NA))
paste_na <- function(..., sep = "") {

  L <- cbind(...)
  ret <- apply(L, 1,
               function(x) paste0(x[!is.na(x)], collapse = sep))
  is.na(ret) <- ret==""
  ret

}


# more verbose approach
# paste_na <- function(..., sep = "") {
#
#   L <- list(...)
#   L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
#   ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
#              gsub(paste0(sep,sep),sep,
#                   do.call(paste,c(L,list(sep=sep)))))
#   is.na(ret) <- ret==""
#   ret
#
# }



#' Unwrap string
#'
#' Convenience function to allow writing text in source with line breaks,
#' saving character without.
#'
#' @param x single character string
#'
#' @return character
#' @export
#'
#' @examples
#' gp_unwrap("This is a very long string
#'           with spaces and line breaks.
#'           but that doesn't matter.")
gp_unwrap <- function(x) {

  x %>% str_split("\n") %>% chuck(1) %>% str_trim %>% paste0(collapse = " ")

}


#' print tibble all rows
#'
#' @param x tibble
#'
#' @return x
#' @export
prinf <- function(x) {

  print(x, n = Inf)
  invisible(x)

}

#' update gptools package
#'
#' Having a fct for that was probably unnecessary, but why not?
#'
#' @return dunno
#' @export
gp_uppck <- function() {

  remotes::install_github("gpatoine/gptools")

}


#' Check if integer
#'
#' See is.wholenumber in http://web.mit.edu/~r/current/lib/R/library/base/html/integer.html
#'
#' @param x numeric
#' @param tol tolerance
#'
#' @return boolean
#' @export
is_whole <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}


#' R-squared
#'
#' @param predicted vector
#' @param observed vector
#'
#' @return numeric
#' @export
gp_r2 <- function(predicted, observed){
  return(1- sum((predicted - observed) ^ 2)/sum((observed - mean(observed))^2))
}


#' Open PDF in Foxit with no lock
#'
#' https://kb.foxitsoftware.com/hc/en-us/articles/360040661611-How-to-make-Foxit-PhantomPDF-Foxit-Reader-not-lock-PDF-file-when-open-it
#'
#' @param path path
#'
#' @return system
#' @export
foxit <- function(path) {

  command <- paste0('"C:/Program Files (x86)/Foxit Software/Foxit PDF Reader/FoxitPDFReader.exe" "', path, '" /A nolock=1')
  system(command)

}


#' @title Quote Words
#'
#' @description
#' \code{qw} Takes an unquoted vector and adds quotes to it like the qw function in perl.
#'
#' @details This is a helper function for data processing. Honestly, I use qw all the time
#' in other languages, and wanted a version for R. Taken from jebyrnes/multifunc
#'
#' @author Jarrett Byrnes.
#' @param ... Any unquoted strings
#'
#'
#' @export
#' @return A vector
#'
#' @examples
#' c("a", "b")
#'
#' qw(a, b)
#'
#'
#'
#' # qw - a helper function that we
#' # will use later to deal with strings
#' # analagous to qw in PERL
qw <- function(...) {
  sapply(match.call()[-1], deparse)
}



#' ignore
#'
#' Can be used to mute view in script (e.g. for sourcing), or comment
#' out a block of code.
#'
#' @param ... expression
#'
#' @return NULL
#' @export
#'
#' @examples view <- ignore
#' ignore({ expr })
#'
ignore <- function(...) {
  invisible(NULL)
}
