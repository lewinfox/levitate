# ---- Distance functions ----

#' Default parameters inherited by other documentation
#'
#' @param a,b The input strings
#' @param useNames Boolean. Use input vectors as row and column names?
#' @param ... Additional arguments to be passed to [stringdist::stringdistmatrix()] or
#'   [stringdist::stringsimmatrix()].
#'
#' @name default-params
#'
#' @seealso [stringdist::stringdist()], [stringdist::stringsim()] for details on the underlying
#'   functions and the additional options available.
NULL

#' Distance between strings
#'
#' Uses [stringdist::stringdistmatrix()] to compute a range of
#' [string distance metrics][stringdist::stringdist-metrics].
#'
#' @section Details:
#' This is a thin wrapper around [stringdist::stringdistmatrix()] and mainly exists to coerce the
#' output into the simplest possible format (via [lev_simplify_matrix()]).
#'
#' The function will return the simplest possible data structure permitted by the length of the
#' inputs `a` and `b`. This will be a scalar if `a` and `b` are length 1, a vector if either (but
#' not both) is length > 1, and a matrix otherwise.
#'
#' @section Other options:
#' In addition to `useNames` [stringdist::stringdistmatrix()] provides a range of options to control
#' the matching, which can be passed using `...`. Refer to the `stringdist` documentation for more
#' information.
#'
#' @inheritParams default-params
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs. See "Details".
#'
#' @export
#'
#' @examples
#' lev_distance("Bilbo", "Frodo")
#' ## [1] 4
#'
#' lev_distance("Bilbo", c("Frodo", "Merry"))
#' ## Frodo Merry
#' ##     4     5
#'
#' lev_distance("Bilbo", c("Frodo", "Merry"), useNames = FALSE)
#' ## [1] 4 5
#'
#' lev_distance(c("Bilbo", "Gandalf"), c("Frodo", "Merry"))
#' ##         Frodo Merry
#' ## Bilbo       4     5
#' ## Gandalf     6     7
lev_distance <- function(a, b, useNames = TRUE, ...) {
  res <- stringdist::stringdistmatrix(a, b, useNames = useNames, ...)
  lev_simplify_matrix(res)
}

#' Similarity ratio between strings
#'
#' @inheritParams default-params
#'
#' @section Details:
#' This is a thin wrapper around [stringdist::stringsimmatrix()] and mainly exists to coerce the
#' output into the simplest possible format (via [lev_simplify_matrix()]).
#'
#' The function will return the simplest possible data structure permitted by the length of the
#' inputs `a` and `b`. This will be a scalar if `a` and `b` are length 1, a vector if either (but
#' not both) is length > 1, and a matrix otherwise.
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs. See "Details".
#'
#' @export
lev_ratio <- function(a, b, useNames = TRUE, ...) {
  # TODO: Where the arguments are different lengths that are not a multiple of each other we get a
  #       warning about fractional argument recycling from `pdist()` which is used by
  #       `stringdist::stringsimmatrix()`. Suppressing the warning like this is bad practice!
  if (length(a) %% length(b) != 0) {
    res <- suppressWarnings(stringdist::stringsimmatrix(a = a, b = b, useNames = useNames, ...))
  } else {
    res <- stringdist::stringsimmatrix(a = a, b = b, useNames = useNames, ...)
  }
  lev_simplify_matrix(res)
}

#' Partial string ratio
#'
#' Find the best `lev_ratio()` between substrings.
#'
#' @inheritParams default-params
#'
#' @section Details:
#' If string `a` has length `len_a` and is shorter than string `b`, this function finds the highest
#' [lev_ratio()] of all the `len_a`-long substrings of `b` (and vice versa).
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs.
#'
#' @export
#'
#' @examples
#' lev_ratio("Bruce Springsteen", "Bruce Springsteen and the E Street Band")
#' ## [1] 0.4358974
#'
#' # Here the two "Bruce Springsteen" strings will match perfectly.
#' lev_partial_ratio("Bruce Springsteen", "Bruce Springsteen and the E Street Band")
#' ## [1] 1
lev_partial_ratio <- function(a, b, useNames = TRUE, ...) {
  inputs <- expand.grid(a = a, b = b)
  scores <- apply(inputs, 1, function(row) internal_lev_partial_ratio(row[1], row[2], useNames = useNames, ...))
  res <- matrix(scores, nrow = length(a), ncol = length(b), dimnames = list(a, b))
  lev_simplify_matrix(res)
}

#' Levenshtein ratio between strings
#'
#' @inheritParams default-params
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs. See "Details".
#'
#' @export
lev_token_sort_ratio <- function(a, b, useNames = TRUE, ...) {
  a <- str_token_sort(a)
  b <- str_token_sort(b)
  res <- lev_ratio(a, b, useNames = useNames, ...)
  lev_simplify_matrix(res)
}

#' Levenshtein ratio between strings
#'
#' @inheritParams default-params
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs. See "Details".
#'
#' @export
lev_token_set_ratio <- function(a, b, useNames = TRUE, ...) {
  inputs <- expand.grid(a = a, b = b)
  scores <- apply(inputs, 1, function(row) internal_lev_token_set_ratio(row[1], row[2], useNames = useNames, ...))
  res <- matrix(scores, nrow = length(a), ncol = length(b), dimnames = list(a, b))
  lev_simplify_matrix(res)
}

# ---- Internal functions ----
#
# `lev_partial_ratio()` and `lev_token_set_ratio()` are hard to vectorise in one go, so in the
# interests of lazy thikning these "internal" versions contain the logic to operate on single-length
# inputs, and the calling functions just `apply()` them as needed.

#' Internal functions
#'
#' [lev_partial_ratio()] and [lev_token_set_ratio()] are hard to vectorise in one go, so in the
#' interests of lazy thikning these "internal" versions contain the logic to operate on
#' single-length inputs, and the calling functions just [apply()] them as needed.
#'
#' @param a,b The input strings. For these "internal" functions these must be length 1
#' @inheritParams default-params
#'
#' @name internal-functions
NULL

#' @describeIn internal-functions See [lev_token_set_ratio()].
internal_lev_token_set_ratio <- function(a, b, useNames = TRUE, ...) {

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
  res <- max(
    lev_ratio(common_tokens, new_a, useNames = useNames, ...),
    lev_ratio(common_tokens, new_b, useNames = useNames, ...),
    lev_ratio(new_a, new_b, useNames = useNames, ...)
  )
  lev_simplify_matrix(res)
}

#' @describeIn internal-functions See [lev_partial_ratio()].
internal_lev_partial_ratio <- function(a, b, useNames = TRUE, ...) {
  # Assume we are only dealing with one string each
  len_a <- nchar(a)
  len_b <- nchar(b)
  if (len_a == len_b) {
    return(lev_ratio(a = a, b = b, useNames = useNames, ...))
  }
  short <- NULL
  long <- NULL
  n <- NULL
  if (len_a < len_b) {
    short <- a
    long <- b
    n <- len_a
  } else {
    short <- b
    long <- a
    n <- len_b
  }
  candidates <- str_all_substrings(long, n)
  scores <- lev_ratio(a = a, b = candidates, useNames = useNames, ...)
  max(scores)
}

# ---- Utilities ----

#' Simplify a matrix
#'
#' Given an input matrix, try and simplify it to a scalar or vector. This requires that one or both
#' of the dimensions are 1. If the matrix has [dimnames()] and the output has more than one item,
#' name the elements according to the longest dimname.
#'
#' @param m A matrix. If `m` is not a matrix it is returned unchanged.
#'
#' @return A scalar, vector or matrix as described above.
lev_simplify_matrix <- function(m) {
  if (!is.matrix(m)) {
    return(m)
  }
  n_row <- nrow(m)
  n_col <- ncol(m)
  if (n_row == 1 || n_col == 1) {
    # We can return a vector
    if (n_row == n_col || is.null(dimnames(m))) {
      # Both are 1 or we don't care about names, or there are no names. Return an unnamed vector.
      return(as.vector(m))
    }
    # The names of the longer axis become the names of the vector
    dim_names <- dimnames(m)
    if (length(dim_names[[1]]) > length(dim_names[[2]])) {
      return(structure(as.vector(m), names = dim_names[[1]]))
    }
    return(structure(as.vector(m), names = dim_names[[2]]))
  }
  # If none of the above apply the matrix can't be simplified
  m
}
