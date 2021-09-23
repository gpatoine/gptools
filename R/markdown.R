


#' Knit with timestamp
#'
#' inspired by https://bookdown.org/yihui/rmarkdown-cookbook/custom-knit.html
#'
#' @param input input
#' @param ... not used
#'
#' @return not sure
#' @export
knit_w_tmst <- function(input, ...) {

  rmarkdown::render(
    input,
    output_file = paste0(xfun::sans_ext(input), tmst(".html")),
    envir = globalenv()
  )

}

