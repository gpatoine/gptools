% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{paste_na}
\alias{paste_na}
\title{Paste NA}
\usage{
paste_na(..., sep = "")
}
\arguments{
\item{...}{character vectors}

\item{sep}{a character string to separate the terms.}
}
\value{
character vector
}
\description{
paste that removes NAs
}
\examples{
paste_na(c(1,2,3, NA), c('a', NA, NA, NA), c('a', 2, "NA", NA), sep = "--")
paste_na(c(1,NA,3, NA), c('a', 2, NA, NA))
}
