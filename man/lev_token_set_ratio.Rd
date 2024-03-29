% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lev-distance.R
\name{lev_token_set_ratio}
\alias{lev_token_set_ratio}
\title{Matching based on common tokens}
\usage{
lev_token_set_ratio(a, b, pairwise = TRUE, useNames = TRUE, ...)
}
\arguments{
\item{a, b}{The input strings}

\item{pairwise}{Boolean. If \code{TRUE}, only the pairwise distances between \code{a} and \code{b} will be
computed, rather than the combinations of all elements.}

\item{useNames}{Boolean. Use input vectors as row and column names?}

\item{...}{Additional arguments to be passed to \code{\link[stringdist:stringdist]{stringdist::stringdistmatrix()}} or
\code{\link[stringdist:stringsim]{stringdist::stringsimmatrix()}}.}
}
\value{
A numeric scalar, vector or matrix depending on the length of the inputs.
}
\description{
Compare stings based on shared tokens.
}
\section{Details}{

Similar to \code{\link[=lev_token_sort_ratio]{lev_token_sort_ratio()}} this function breaks the input down into tokens. It then
identifies any common tokens between strings and creates three new strings:

\if{html}{\out{<div class="sourceCode">}}\preformatted{x <- \{common_tokens\}
y <- \{common_tokens\}\{remaining_unique_tokens_from_string_a\}
z <- \{common_tokens\}\{remaining_unique_tokens_from_string_b\}
}\if{html}{\out{</div>}}

and performs three pairwise \code{\link[=lev_ratio]{lev_ratio()}} calculations between them (\code{x} vs \code{y}, \code{y} vs \code{z} and
\code{x} vs \code{z}). The highest of those three ratios is returned.
}

\examples{
x <- "the quick brown fox jumps over the lazy dog"
y <- "my lazy dog was jumped over by a quick brown fox"

lev_ratio(x, y)

lev_token_sort_ratio(x, y)

lev_token_set_ratio(x, y)
}
\seealso{
\code{\link[=lev_token_sort_ratio]{lev_token_sort_ratio()}}
}
