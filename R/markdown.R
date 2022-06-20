


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


#' A better knitr::stitch
#'
#' I had issues with stich which was behaving strangely and would not embed figures,
#' or would only work in the root folder (which is not what I wanted), or wouldn't
#' work with changed output file name. So here's a
#' modified version that runs knitr::stitch on the current script and saves the
#' html report to a stitch folder in the project.
#'
#' NOTE there's something weird happening with the title, but whatever.
#' could use envir = new.env() in case there's an issue, but then might miss things from Rprofile
#'
#' @param script Path to the R script. Default is the current document.
#' @param template Path of the template to use. By default, the Rnw template in
#'   this package; there is also an HTML template in \pkg{knitr}.
#' @param output Output filename, passed to \code{\link{knit}}). By default,
#'   the base filename of the script is used. @param template
#' @param text see knitr
#' @param envir see knitr
#' @param rm_view don't run View function calls
#'
#' @return output path
#' @export
gp_stitch <- function(script = NULL,
                      template = system.file("misc", "knitr-template.Rmd", package = "knitr"),
                      output = NULL, text = NULL, envir = parent.frame(),
                      rm_view = TRUE) {

  # NOTE the Rhtml template also works

  # cancel View: bit challenging to do, using tibble::view in scripts instead
  # won't work cause knitted documents have interactive() == TRUE
  # Could also be replace by print or something
  if(rm_view) {
    assign("View", function(...) invisible(NULL), envir = parent.frame())
    on.exit(rm(View, envir = parent.frame()))
  }

  if (is.null(script) && interactive() && !is.null(rstudioapi::documentPath())) {
    script <- rstudioapi::documentPath()
  }

  tmpdir <- tempdir() # do I need to delete it manually after?

  lines = if (nosrc <- is.null(text)) xfun::read_utf8(script) else knitr::split_lines(text)

  if (knitr:::comment_to_var(lines[1L], ".knitr.title", "^#+ *title:", envir)) lines = lines[-1L]
  if (knitr:::comment_to_var(lines[1L], ".knitr.author", "^#+ *author:", envir)) lines = lines[-1L]
  input = basename(template)
  input = xfun::with_ext(basename(if (nosrc) script else tempfile()), xfun::file_ext(input))
  txt = xfun::read_utf8(template)
  i = grep("%sCHUNK_LABEL_HERE", txt)
  if (length(i) != 1L) stop("Wrong template for stitch: ", template)
  h = sub("CHUNK_LABEL_HERE", "", txt[i])
  j = grep(knitr:::.sep.label, lines)
  if (length(j) == 0) {
    lines = c(sprintf(h, "auto-report"), lines)
  }  else {
    lines[j] = sprintf(h, gsub(.sep.label, "\\3", lines[j]))
    if (j[1] != 1L) lines = c(sprintf(h, ""), lines)
  }
  txt[i] = knitr:::one_string(lines)
  knitr::opts_chunk$set(
    fig.align = "center", par = TRUE, fig.width = 9, fig.height = 7,
    # out.width="80%",
    # fig.path = here::here("stitch/figure", gsub("[^[:alnum:]]", "-", input)) # changed fig path
    fig.path = file.path(tmpdir, "figure")
  )

  on.exit(knitr::opts_chunk$restore(), add = TRUE)
  knitr::knit_hooks$set(par = function(before, options, envir) {
    if (before) par(mar = c(4, 4, 0.1, 0.1), cex.lab = 0.95, cex.axis = 0.9,
                    mgp = c(2, 0.7, 0), tcl = -0.3, las = 1)
  })
  on.exit(knitr::knit_hooks$restore(), add = TRUE)
  # out = knitr::knit(input, hptmst("stitch", tools::file_path_sans_ext(basename(input)), "md"), envir = envir, text = txt) #changed output path
  out = knitr::knit(input, file.path(tmpdir, xfun::with_ext(basename(input), "md")), envir = envir, text = txt)

  # switch(file_ext(out), tex = {
  #   tinytex::latexmk(out)
  #   message("PDF output at: ", with_ext(out, "pdf"))
  # }, md = {
  # out.html = xfun::with_ext(out, "html")

  if (!dir.exists(here::here("stitch"))) dir.create(here::here("stitch"))
  out.html = hptmst("stitch", tools::file_path_sans_ext(basename(input)), "html")
  markdown::markdownToHTML(out, out.html)
  message("HTML output at: ", out.html)
  # })

  invisible(out.html)
}






