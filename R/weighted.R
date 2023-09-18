#' Weighted token similarity measure
#'
#' Computes similarity but allows you to assign weights to specific tokens. This
#' is useful, for example, when you have a frequently-occurring string that
#' doesn't contain useful information. See examples.
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
#' @param weights List of token weights. For example,
#'   `weights = list(foo = 0.9, bar = 0.1)`. Any tokens omitted from `weights`
#'   will be given a weight of 1.
#'
#' @return A float
#' @export
#'
#' @examples
#' weighted_lev_ratio("jim ltd", "tim ltd")
#'
#' weighted_lev_ratio("tim ltd", "jim ltd", weights = list(ltd = 0.1))
weighted_lev_ratio <- function(a, b, weights = list(), ...) {
  if (length(a) != 1 || length(b) != 1) {
    rlang::abort("`a` and `b` must be length 1")
  }
  a_tokens <- unlist(str_tokenise(a))
  b_tokens <- unlist(str_tokenise(b))

  token_lev_distance <- mapply(lev_distance, a_tokens, b_tokens, MoreArgs = ...)
  max_edit_distance <- mapply(function(a, b) max(nchar(a), nchar(b)), a_tokens, b_tokens)

  # Apply weights to matched tokens
  a_weights <- weights[a_tokens]
  b_weights <- weights[b_tokens]
  # Replace NULL with 1 (i.e. no weighting) and unpack into vectors
  a_weights <- unlist(replace(a_weights, which(sapply(a_weights, is.null)), 1))
  b_weights <- unlist(replace(b_weights, which(sapply(b_weights, is.null)), 1))
  # We want the min weight for each token to ensure we capture occurrences in
  # `a` and `b`
  final_weights <- pmin(a_weights, b_weights)

  # The similarity score is (1 - (edit_distance / max_edit_distance)), after
  # weighting
  n <- token_lev_distance * final_weights
  d <- max_edit_distance * final_weights
  1 - (sum(n) / sum(d))
}
