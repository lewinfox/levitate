#' Sum `nchar()`
#'
#' Vectorised Levenshtein ratio calculations require us to add the number of characters in two
#' strings together. This function is vectorised over `a` and `b`. Unless both inputs are scalar the
#' result will be a matrix to simplify calculations in [lev_ratio()].
#'
#' @param a,b The input strings. Vectorised input is accepted.
#'
#' @return The result of `nchar(a) + nchar(b)` for all combinations of the elements of `a` and `b`.
#'   If `a` and `b` are length 1 the result is a scalar (i.e. a length 1 vector) otherwise a matrix.
#'
#' @examples
#' \dontrun{
#' str_char_sum("cat", "mouse")
#' ## [1] 8
#'
#' str_char_sum(c("cat", "cats"), "mouse")
#' ##      mouse
#' ## cat      8
#' ## cats     9
#'
#' str_char_sum(c("cat", "cats"), c("mouse", "mice"))
#' ##      mouse mice
#' ## cat      8    7
#' ## cats     9    8
#' }
str_char_sum <- function(a, b) {
  x <- expand.grid(nchar(a), nchar(b))
  res <- matrix(x[, 1] + x[, 2], nrow = length(a), ncol = length(b), dimnames = list(a, b))
  if (nrow(res) == 1 && ncol(res) == 1) {
    return(as.vector(res))
  }
  res
}

#' Find all substrings of a given length
#'
#' @param x The input string.
#' @param n The length of the desired substrings.
#'
#' @return A character vector containing all the length `n` substrings of `x`.
#'
#' @examples
#' \dontrun{
#' str_all_substrings("hello", 3)
#' ## [1] "hel" "ell" "llo"
#' }
str_all_substrings <- function(x, n) {
  unique(substring(x, 1:(nchar(x) - n + 1), n:nchar(x)))
}

#' Tokenise a string
#'
#' Splits an input string into tokens. A wrapper for [strsplit()] with a predefined split regex of
#' `[^[:alnum:]]`, i.e. anything except `[a-zA-Z0-9]`.
#'
#' @param x The input string.
#' @param split The regular expression to split on. See [strsplit()].
#'
#' @return A list containing one character vector for each element of `x`.
#'
#' @examples
#' \dontrun{
#' str_tokenise("this is nice isn't it")
#' ## [[1]]
#' ## [1] "this" "is"   "nice" "isn"  "t"    "it"
#'
#' str_tokenise(c("hello R", "goodbye R"))
#'
#' ## [[1]]
#' ## [1] "hello" "R"
#'
#' ## [[2]]
#' ## [1] "goodbye" "R"
#' }
str_tokenise <- function(x, split = "[^[:alnum:]]") {
  # TODO: Is this the best regex to use? In the example given above, "isn't" splits into two.
  strsplit(x, split)
}

#' Tokenise and sort a string
#'
#' Given an input string, tokenise it (using [str_tokenise()]), sort the tokens alphabetically and
#' return the result as a single space-separated string.
#'
#' @param x The input string
#' @param ignore_case Boolean. If `TRUE`, the input will be lowercased before processing.
#'
#' @return A character vector the same length as `x` containing the sorted tokens.
#'
#' @examples
#' \dontrun{
#' str_token_sort("R is great fun")
#' ## [1] "fun great is R"
#'
#' str_token_sort(c("R is great fun", "string manipulation is less so"))
#' ## [1] "fun great is R"                 "is less manipulation so string"
#' }
str_token_sort <- function(x, ignore_case = TRUE) {
  if (ignore_case) {
    x <- tolower(x)
  }
  tokens <- str_tokenise(x)
  sorted_tokens <- lapply(tokens, sort)
  sapply(sorted_tokens, paste, collapse = " ")
}
