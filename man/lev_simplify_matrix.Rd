% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lev-distance.R
\name{lev_simplify_matrix}
\alias{lev_simplify_matrix}
\title{Simplify a matrix}
\usage{
lev_simplify_matrix(m)
}
\arguments{
\item{m}{A matrix. If \code{m} is not a matrix it is returned unchanged.}
}
\value{
A scalar, vector or matrix as described above.
}
\description{
Given an input matrix, try and simplify it to a scalar or vector. This requires that one or both
of the dimensions are 1. If the matrix has \code{\link[=dimnames]{dimnames()}} and the output has more than one item,
name the elements according to the longest dimname.
}
\keyword{internal}
