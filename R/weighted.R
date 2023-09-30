#' Weighted token similarity measure
#'
#' Computes similarity but allows you to assign weights to specific tokens. This is useful, for
#' example, when you have a frequently-occurring string that doesn't contain useful information. See
#' examples.
#'
#' # Details
#'
#' The algorithm used here is as follows:
#'
#' * Tokenise the input strings
#' * Compute the edit distance between each pair of tokens
#' * Compute the maximum edit distance between each pair of tokens
#' * Apply any weights from the `weights` argument
#' * Return `1 - (sum(weighted_edit_distances) / sum(weighted_max_edit_distance))`
#'
#' @inheritParams default-params
#' @param weights List of token weights. For example, `weights = list(foo = 0.9, bar = 0.1)`. Any
#'   tokens omitted from `weights` will be given a weight of 1.
#'
#' @return A float
#' @export
#'
#' @family weighted token functions
#'
#' @examples
#' lev_weighted_token_ratio("jim ltd", "tim ltd")
#'
#' lev_weighted_token_ratio("tim ltd", "jim ltd", weights = list(ltd = 0.1))
lev_weighted_token_ratio <- function(a, b, weights = list(), ...) {
  if (length(a) != 1 || length(b) != 1) {
    rlang::abort("`a` and `b` must be length 1")
  }
  a_tokens <- unlist(str_tokenise(a))
  b_tokens <- unlist(str_tokenise(b))

  # If the token lists aren't the same length we will pad the shorter list with empty strings
  if (length(a_tokens) > length(b_tokens)) {
    b_tokens <- c(b_tokens, rep("", length(a_tokens) - length(b_tokens)))
  } else if (length(a_tokens) < length(b_tokens)) {
    a_tokens <- c(a_tokens, rep("", length(b_tokens) - length(a_tokens)))
  }

  token_lev_distances <- mapply(lev_distance, a_tokens, b_tokens, MoreArgs = ...)

  #  Weights are applied where
  #
  #  * a token is in the `weights` list
  #  * AND the token appears in the same position in a and b.
  #  * OR the token appears in a OR b and the corresponding token is missing (which has the effect
  #    of reducing the impact of tokens that appear in one string but not the other).
  weights_to_apply <- mapply(
    function(token_a, token_b) {
      if (token_a == token_b && token_a %in% names(weights)) {
        weights[[token_a]]
      } else if (token_a == "" && token_b %in% names(weights)) {
        weights[[token_b]]
      } else if (token_b == "" && token_a %in% names(weights)) {
        weights[[token_a]]
      } else {
        1
      }
    },
    a_tokens,
    b_tokens
  )

  # The similarity score is (1 - (edit_distance / max_edit_distance)), after weighting.
  weighted_edit_distances <- token_lev_distances * weights_to_apply
  weighted_max_edit_distances <- mapply(function(a, b) max(nchar(a), nchar(b)), a_tokens, b_tokens) * weights_to_apply

  1 - (sum(weighted_edit_distances) / sum(weighted_max_edit_distances))
}

#' Weighted version of lev_token_sort_ratio()
#'
#' This function tokenises inputs, sorts tokens and computes similarities for each pair of tokens.
#' Similarity scores are weighted based on the `weights` argument, and a total similarity score is
#' returned in the same manner as [lev_weighted_token_ratio()].
#'
#' @inheritParams default-params
#' @inheritParams lev_weighted_token_ratio
#'
#' @return Float
#' @export
#' @family weighted token functions
#' @seealso [lev_token_sort_ratio()]
lev_weighted_token_sort_ratio <- function(a, b, weights = list(), ...) {
  if (length(a) != 1 || length(b) != 1) {
    rlang::abort("`a` and `b` must be length 1")
  }
  lev_weighted_token_ratio(str_token_sort(a), str_token_sort(b), weights = weights, ...)
}

#' Weighted version of `lev_token_set_ratio()`
#'
#' @inheritParams default-params
#' @inheritParams lev_weighted_token_ratio
#' @return Float
#' @family weighted token functions
#' @seealso [lev_token_set_ratio()]
#' @export
lev_weighted_token_set_ratio <- function(a, b, weights = list(), ...) {
  if (length(a) != 1 || length(b) != 1) {
    rlang::abort("`a` and `b` must be length 1")
  }

  token_a <- unlist(str_tokenise(a))
  token_b <- unlist(str_tokenise(b))
  common_tokens <- sort(intersect(token_a, token_b))
  unique_token_a <- sort(setdiff(token_a, token_b))
  unique_token_b <- sort(setdiff(token_b, token_a))

  # Construct two new strings of the form {sorted_common_tokens}{sorted_remainder_a/b} and return
  # a lev_weighted_token_ratio() on those
  new_a <- paste(c(common_tokens, unique_token_a), collapse = " ")
  new_b <- paste(c(common_tokens, unique_token_b), collapse = " ")

  # We want the max of the three pairwise comparisons between `common_tokens`, `new_a` and `new_b`.
  # For this to work properly we need to stick `common_tokens` back together into a single string.
  common_tokens <- paste(common_tokens, collapse = " ")
  res <- max(
    lev_weighted_token_ratio(common_tokens, new_a, weights = weights, ...),
    lev_weighted_token_ratio(common_tokens, new_b, weights = weights, ...),
    lev_weighted_token_ratio(new_a, new_b, weights = weights, ...)
  )

  res
}
