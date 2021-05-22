#' Find all substrings of a given length
#'
#' @param x The input string. Non-character inputs will be coerced with [as.character()].
#' @param n The length of the desired substrings.
#'
#' @return A character vector containing all the length `n` substrings of `x`. If `x` has length >
#'   then a list is returned containing an entry for each input element.
#'
#' @keywords internal
str_all_substrings <- function(x, n) {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  if (length(x) > 1) {
    return(lapply(x, str_all_substrings, n = n))
  }
  if (n < 1) {
    rlang::abort("`n` must be a positive integer")
  }
  unique(substring(x, 1:(nchar(x) - n + 1), n:nchar(x)))
}

#' Tokenise a string
#'
#' Splits an input string into tokens. A wrapper for [strsplit()] with a predefined split regex of
#' `[^[:alnum:]]`, i.e. anything except `[a-zA-Z0-9]`.
#'
#' @param x The input string. Non-character inputs will be coerced with [as.character()].
#' @param split The regular expression to split on. See [strsplit()].
#'
#' @return A list containing one character vector for each element of `x`.
#'
#' @keywords internal
str_tokenise <- function(x, split = "[^[:alnum:]]+") {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  # TODO: Is this the best regex to use? In the example given above, "isn't" splits into two.
  strsplit(x, split)
}

#' Tokenise and sort a string
#'
#' Given an input string, tokenise it (using [str_tokenise()]), sort the tokens alphabetically and
#' return the result as a single space-separated string.
#'
#' @param x The input string. Non-character inputs will be coerced with [as.character()].
#'
#' @return A character vector the same length as `x` containing the sorted tokens.
#'
#' @keywords internal
str_token_sort <- function(x) {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  tokens <- str_tokenise(x)
  sorted_tokens <- lapply(tokens, sort)
  sapply(sorted_tokens, paste, collapse = " ")
}
