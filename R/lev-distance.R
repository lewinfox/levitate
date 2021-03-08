# ---- Distance functions ----

#' Default parameters inherited by other documentation
#'
#' @param a,b The input strings
#' @param pairwise Boolean. If `TRUE`, only the pairwise distances between `a` and `b` will be
#'   computed, rather than the combinations of all elements.
#' @param useNames Boolean. Use input vectors as row and column names?
#' @param ... Additional arguments to be passed to [stringdist::stringdistmatrix()] or
#'   [stringdist::stringsimmatrix()].
#'
#' @name default-params
#'
#' @seealso [stringdist::stringdist()], [stringdist::stringsim()] for details on the underlying
#'   functions and the additional options available.
NULL

#' String distance metrics
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
#'
#' lev_distance("Bilbo", c("Frodo", "Merry"))
#'
#' lev_distance("Bilbo", c("Frodo", "Merry"), useNames = FALSE)
#'
#' lev_distance(c("Bilbo", "Gandalf"), c("Frodo", "Merry"))
lev_distance <- function(a, b, pairwise = TRUE, useNames = TRUE, ...) {
  if (pairwise) {
    len_a <- length(a)
    len_b <- length(b)
    if (len_a != len_b && len_a > 1 && len_b > 1) {
      rlang::abort(
        "`a` and `b` must be the same length (or one must be length 1) for pairwise comparison",
        "levitate_length_mismatch"
      )
    }
    # The pairwise argument is only relevant where both inputs are longer than 1.
    if (len_a > 1 && len_b > 1) {
      res <- stringdist::stringdist(a, b, ...)
      return(res)
    }
  }
  res <- stringdist::stringdistmatrix(a, b, useNames = useNames, ...)
  lev_simplify_matrix(res)
}

#' String similarity ratio
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
#' @return A numeric scalar, vector or matrix depending on the length of the inputs.
#'
#' @export
#'
#' @examples
#' lev_ratio("Bilbo", "Frodo")
#'
#' lev_ratio("Bilbo", c("Frodo", "Merry"))
#'
#' lev_ratio("Bilbo", c("Frodo", "Merry"), useNames = FALSE)
#'
#' lev_ratio(c("Bilbo", "Gandalf"), c("Frodo", "Merry"))
lev_ratio <- function(a, b, pairwise = TRUE, useNames = TRUE, ...) {
  if (pairwise) {
    len_a <- length(a)
    len_b <- length(b)
    if (len_a != len_b && len_a > 1 && len_b > 1) {
      rlang::abort(
        "`a` and `b` must be the same length (or one must be length 1) for pairwise comparison",
        "levitate_length_mismatch"
      )
    }
    # The pairwise argument is only relevant where both inputs are longer than 1.
    if (len_a > 1 && len_b > 1) {
      res <- stringdist::stringsim(a, b, ...)
      return(res)
    }
  }
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

#' Ratio of the best-matching substring
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
#' #> [1] 0.4358974
#'
#' # Here the two "Bruce Springsteen" strings will match perfectly.
#' lev_partial_ratio("Bruce Springsteen", "Bruce Springsteen and the E Street Band")
#' #> [1] 1
lev_partial_ratio <- function(a, b, pairwise = TRUE, useNames = TRUE, ...) {
  if (pairwise) {
    len_a <- length(a)
    len_b <- length(b)
    if (len_a != len_b && len_a > 1 && len_b > 1) {
      rlang::abort(
        "`a` and `b` must be the same length (or one must be length 1) for pairwise comparison",
        "levitate_length_mismatch"
      )
    }
    output_names <- if (len_a > len_b) a else b
    inputs <- data.frame(a = a, b = b, stringsAsFactors = FALSE)
  } else {
    inputs <- expand.grid(a = a, b = b)
  }
  scores <- apply(
    inputs,
    1,
    function(row) internal_lev_partial_ratio(row[1], row[2], pairwise = pairwise, useNames = useNames, ...)
  )

  if (pairwise) {
    if (useNames && ((len_a == 1 && len_b > 1) || (len_a > 1 && len_b == 1))) {
      names(scores) <- output_names
    }
    return(scores)
  }
  res <- matrix(scores, nrow = length(a), ncol = length(b), dimnames = list(a, b))
  lev_simplify_matrix(res)
}

#' Ordered token matching
#'
#' Compares strings by tokenising them, sorting the tokens alphabetically and then computing the
#' [lev_ratio()] of the result. This means that the order of words is irrelevant which can be
#' helpful in some circumstances.
#'
#' @inheritParams default-params
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs.
#'
#' @export
#'
#' @seealso [lev_token_set_ratio()]
#'
#' @examples
#' x <- "Episode IV - Star Wars: A New Hope"
#' y <- "Star Wars Episode IV - New Hope"
#'
#' # Because the order of words is different the simple approach gives a low match ratio.
#' lev_ratio(x, y)
#'
#' # The sorted token approach ignores word order.
#' lev_token_sort_ratio(x, y)
lev_token_sort_ratio <- function(a, b, pairwise = TRUE, useNames = TRUE, ...) {
  # TODO: We should have the option to supply our own tokeniser function / regex here.
  #       * Add an arg `tokeniser`
  #       * It it's a character, assume regex and pass to `strsplit()`
  #       * If it's a function, run the inputs through it.
  #       * Write a vignette.
  a <- str_token_sort(a)
  b <- str_token_sort(b)
  res <- lev_ratio(a, b, pairwise = pairwise, useNames = useNames, ...)
  lev_simplify_matrix(res)
}

#' Matching based on common tokens
#'
#' Compare stings based on shared tokens.
#'
#' @section Details:
#' Similar to [lev_token_sort_ratio()] this function breaks the input down into tokens. It then
#' identifies any common tokens between strings and creates three new strings:
#'
#' ```
#' x <- {common_tokens}
#' y <- {common_tokens}{remaining_unique_tokens_from_string_a}
#' z <- {common_tokens}{remaining_unique_tokens_from_string_b}
#' ```
#'
#' and performs three pairwise [lev_ratio()] calculations between them (`x` vs `y`, `y` vs `z` and
#' `x` vs `z`). The highest of those three ratios is returned.
#'
#' @inheritParams default-params
#'
#' @return A numeric scalar, vector or matrix depending on the length of the inputs.
#'
#' @seealso [lev_token_sort_ratio()]
#'
#' @export
#'
#' @examples
#' x <- "the quick brown fox jumps over the lazy dog"
#' y <- "my lazy dog was jumped over by a quick brown fox"
#'
#' lev_ratio(x, y)
#'
#' lev_token_sort_ratio(x, y)
#'
#' lev_token_set_ratio(x, y)
lev_token_set_ratio <- function(a, b, pairwise = TRUE, useNames = TRUE, ...) {
  if (pairwise) {
    len_a <- length(a)
    len_b <- length(b)
    if (len_a != len_b && len_a > 1 && len_b > 1) {
      rlang::abort(
        "`a` and `b` must be the same length (or one must be length 1) for pairwise comparison",
        "levitate_length_mismatch"
      )
    }
    output_names <- if (len_a > len_b) a else b
    inputs <- data.frame(a = a, b = b, stringsAsFactors = FALSE)
  } else {
    inputs <- expand.grid(a = a, b = b)
  }
  scores <- apply(
    inputs,
    1,
    function(row) internal_lev_token_set_ratio(row[1], row[2], pairwise = pairwise, useNames = useNames, ...)
  )
  if (pairwise) {
    if (useNames && ((len_a == 1 && len_b > 1) || (len_a > 1 && len_b == 1))) {
      names(scores) <- output_names
    }
    return(scores)
  }
  res <- matrix(scores, nrow = length(a), ncol = length(b), dimnames = list(a, b))
  lev_simplify_matrix(res)
}

# ---- Internal functions ----
#
# `lev_partial_ratio()` and `lev_token_set_ratio()` are hard to vectorise in one go, so in the
# interests of lazy thinking these "internal" versions contain the logic to operate on single-length
# inputs, and the calling functions just `apply()` them as needed.

#' Internal functions
#'
#' [lev_partial_ratio()] and [lev_token_set_ratio()] are hard to vectorise in one go, so in the
#' interests of lazy thinking these "internal" versions contain the logic to operate on
#' single-length inputs, and the calling functions just [apply()] them as needed.
#'
#' @param a,b The input strings. For these "internal" functions these must be length 1
#' @inheritParams default-params
#'
#' @name internal-functions
NULL

#' @describeIn internal-functions See [lev_token_set_ratio()].
internal_lev_token_set_ratio <- function(a, b, pairwise = TRUE, useNames = !pairwise, ...) {

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
    lev_ratio(common_tokens, new_a, pairwise = TRUE, useNames = useNames, ...),
    lev_ratio(common_tokens, new_b, pairwise = TRUE, useNames = useNames, ...),
    lev_ratio(new_a, new_b, pairwise = TRUE, useNames = useNames, ...)
  )
  lev_simplify_matrix(res)
}

#' @describeIn internal-functions See [lev_partial_ratio()].
internal_lev_partial_ratio <- function(a, b, pairwise = TRUE, useNames = !pairwise, ...) {
  # Assume we are only dealing with one string each
  len_a <- nchar(a)
  len_b <- nchar(b)
  if (len_a == len_b) {
    return(lev_ratio(a = a, b = b, pairwise = pairwise, useNames = useNames, ...))
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
  scores <- lev_ratio(a = a, b = candidates, pairwise = FALSE, useNames = useNames, ...)
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
      # Both are 1 so we don't care about names, or there are no names. Return an unnamed vector.
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
