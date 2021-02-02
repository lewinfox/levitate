# ---- Distance functions ----

#' Default parameters inherited by other documentation
#'
#' @param a,b The input strings
#' @param preprocessor Function or `NULL`. If a function is passed, each input will be passed
#'   through it before analysis starts. Typical use cases are removing punctuation and standardising
#'   case. See `vignette("preprocessors", package = "levitate")` for more details.
#' @param pairwise Boolean. If `TRUE` the function will try and compare the strings pairwise (i.e.
#'   `a[1]` with `b[1]`, `a[2]` with `b[2]`, etc.) and return a vector of the results rather than
#'   comparing every input with every output. For this to work, `a` `b` must be the same length or
#'   one of them must be length 1.
#'
#' @name levenshtein-default-params
NULL

#' Edit distance between strings
#'
#' Refer to [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance) on
#' Wikipedia for more details.
#'
#' @inheritParams levenshtein-default-params
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs. See "Details".
#'
#' @export
lev_distance <- function(a, b, preprocessor = preprocessor_default(), pairwise = FALSE) {
  if (is.function(preprocessor)) {
    a <- preprocessor(a)
    b <- preprocessor(b)
  }
  # TODO: `adist()` has a bunch of options, some of which overlap with the preprocessor argument.
  #       What's the best approach?
  res <- structure(utils::adist(a, b), dimnames = list(a, b))
  if (nrow(res) == 1 && ncol(res) == 1) {
    return(as.vector(res))
  }
  if (pairwise) {
    return(lev_pairwise_result(res))
  }
  res
}

#' Levenshtein ratio between strings
#'
#' @inheritParams levenshtein-default-params
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs. See "Details".
#'
#' @export
lev_ratio <- function(a, b, preprocessor = preprocessor_default(), pairwise = FALSE) {
  if (is.function(preprocessor)) {
    a <- preprocessor(a)
    b <- preprocessor(b)
  }
  c <- str_char_sum(a, b)
  d <- lev_distance(a, b, preprocessor = preprocessor)
  # TODO: The Python formulation is different:
  #
  #         res <- 2 * matches / c
  #
  #       The R version causes confusion with short input, for example lev_ratio("cat", "rat")
  #       (which everyone will agree should be 2/3 (0.667)) instead gives 5/6 (0.833). The version
  #       of the formula used here gives intuitively "incorrect" results for smaller input but takes
  #       advantage of the adist() function and avoids having to write a matcher.
  #
  #       This is a consequence of the fact that Python's `fuzzywuzzy` counts matches rather than
  #       distances (at least the pure Python version without `python-Levenshtein` installed).
  res <- (c - d) / c
  # Any instances where 2 * distance is greater than the total length should be zero. These are
  # strings that have nothing in common.
  res[2 * d >= c] <- 0
  if (pairwise) {
    return(lev_pairwise_result(res))
  }
  res
}

#' Levenshtein ratio between strings
#'
#' @inheritParams levenshtein-default-params
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs. See "Details".
#'
#' @export
lev_partial_ratio_single_string <- function(a, b, preprocessor = preprocessor_default()) {
  if (is.function(preprocessor)) {
    a <- preprocessor(a)
    b <- preprocessor(b)
  }
  len_a <- nchar(a)
  len_b <- nchar(b)
  # If the strings are the same length (or missing) we use the normal `lev_ratio()`
  if (len_a == len_b || len_a == 0 || len_b == 0) {
    return(lev_ratio(a, b, preprocessor = preprocessor))
  }
  # Otherwise, find out which is the shorter and longer string
  candidates <- NULL
  ratios <- NULL
  if (len_a < len_b) {
    candidates <- str_all_substrings(b, len_a)
    ratios <- lev_ratio(a, candidates, preprocessor = preprocessor)
  } else {
    candidates <- str_all_substrings(a, len_b)
    ratios <- lev_ratio(b, candidates, preprocessor = preprocessor)
  }
  winner <- which(ratios == max(ratios))
  ratios[winner]
}

#' Levenshtein ratio between strings
#'
#' @inheritParams levenshtein-default-params
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs. See "Details".
#'
#' @export
lev_partial_ratio <- function(a, b, preprocessor = preprocessor_default(), pairwise = FALSE) {
  res <- lev_do_vectorised(a = a, b = b, FUN = lev_partial_ratio_single_string, preprocessor = preprocessor)
  if (pairwise) {
    return(lev_pairwise_result(res))
  }
  res
}

#' Levenshtein ratio between strings
#'
#' @inheritParams levenshtein-default-params
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs. See "Details".
#'
#' @export
lev_token_sort_ratio <- function(a, b, preprocessor = preprocessor_default(), pairwise = FALSE) {
  if (is.function(preprocessor)) {
    a <- preprocessor(a)
    b <- preprocessor(b)
  }
  a <- str_token_sort(a)
  b <- str_token_sort(b)
  res <- lev_ratio(a, b, preprocessor = preprocessor)
  if (pairwise) {
    return(lev_pairwise_result(res))
  }
  res
}

#' Levenshtein ratio between strings
#'
#' @inheritParams levenshtein-default-params
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs. See "Details".
#'
#' @export
lev_token_set_ratio <- function(a, b, preprocessor = preprocessor_default()) {
  if (is.function(preprocessor)) {
    a <- preprocessor(a)
    b <- preprocessor(b)
  }

  # TODO: Vectorise
  if (!(length(a) == 1 && length(b) == 1)) {
    stop("`a` and `b` must be length 1")
  }

  token_a <- unlist(str_tokenise(a))
  token_b <- unlist(str_tokenise(b))
  common_tokens <- sort(intersect(token_a, token_b))
  unique_token_a <- sort(setdiff(token_a, token_b))
  unique_token_b <- sort(setdiff(token_b, token_a))

  # Construct two new strings of the form {sorted_common_tokens}{sorted_remainder_a/b} and return
  # a lev_ratio() on those
  new_a <- paste(c(common_tokens, unique_token_a), collapse = " ")
  new_b <- paste(c(common_tokens, unique_token_b), collapse = " ")

  # We want the max of the three pairwise comparisons between `common_tokens`, `new_a` and `new_b`.
  # For this to work properly we need to stick `common_tokens` back together into a single string.
  common_tokens <- paste(common_tokens, collapse = " ")
  max(
    lev_ratio(common_tokens, new_a, preprocessor = preprocessor),
    lev_ratio(common_tokens, new_b, preprocessor = preprocessor),
    lev_ratio(new_a, new_b, preprocessor = preprocessor)
  )
}

# ---- Utilities ----

#' Calculate over vectorised input
#'
#' This is a utility function and is not intend to be called directly. Some operations are tricky
#' to vecotorise natively without ugly code, hence the need to break this out into a separate
#' function.
#'
#' @param a,b Character vectors.
#' @param FUN Function to be vectorised.
#' @param ... Other arguments to be passed to `FUN`.
#'
#' @return A matrix.
lev_do_vectorised <- function(a, b, FUN, ...) {
  if (length(a) == 1 && length(b) == 1) {
    return(FUN(a = a, b = b, ...))
  }
  pairs <- expand.grid(a = a, b = b)
  result <- apply(pairs, 1, function(row) FUN(a = row["a"], b = row["b"], ...))
  structure(matrix(result, nrow = length(a), ncol = length(b)), dimnames = list(a, b))
}

#' Convert a matrix into a pairwise result
#'
#' @section Details:
#' Given an input matrix, return only the diagonal. If either [nrow()] or [ncol()] are 1, return a
#' vector. This is a utility function and should not be called directly. Effectively this tries to
#' return the diagonal of a matrix and throws errors if various assumptions do not hold. The three
#' scenarios considered are:
#'
#' 1. The matrix is 1 x 1: Easy, just convert it to a length 1 vector and return.
#' 2. The matrix is 1 x n or n x 1. Convert the single row or column to a vector and return. The
#'    elements of the vector will be named using whichever of the input vectors has length > 1.
#' 3. The matrix is n x n. If the number of rows and columns are the same, return the diagonal as an
#'    (unnamed) vector. If the number of rows and columns are different an error will be thrown.
#'
#' @param m A matrix
#'
#' @return A scalar or vector - see "Details".
lev_pairwise_result <- function(m) {
  # TODO: Scalar input is probably OK, just return it.
  if (!is.matrix(m)) {
    rlang::abort("`m` must be a matrix")
  }
  rows <- nrow(m)
  cols <- ncol(m)
  if (rows == cols) {
    return(diag(m))
  }
  if (rows == 1 && cols > 1) {
    res <- structure(m[1, ], names = colnames(m))
  } else if (cols == 1 && rows > 1) {
    res <- structure(m[, 1], names = rownames(m))
  } else {
    msg <- glue::glue("Result has {{.val {rows}}} rows and {{.val {cols}}} columns. ",
                      "For a pairwise result the number of rows and columns must match, ",
                      "or one or both must be 1.")
    cli::cli_alert_danger(msg)
    rlang::abort("Cannot compute pairwise result")
  }
  return(res)
}
