% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string-utils.R
\name{str_all_substrings}
\alias{str_all_substrings}
\title{Find all substrings of a given length}
\usage{
str_all_substrings(x, n)
}
\arguments{
\item{x}{The input string. Non-character inputs will be coerced with \code{\link[=as.character]{as.character()}}.}

\item{n}{The length of the desired substrings.}
}
\value{
A character vector containing all the length \code{n} substrings of \code{x}. If \code{x} has length >
then a list is returned containing an entry for each input element.
}
\description{
Find all substrings of a given length
}
\examples{
\dontrun{
str_all_substrings("hello", 3)
#> [1] "hel" "ell" "llo"

str_all_substrings(rep("hello", 2), 4)
#> [[1]]
#> [1] "hell" "ello"
#>
#> [[2]]
#> [1] "hell" "ello"
}
}