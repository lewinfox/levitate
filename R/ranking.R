#' Score multiple candidate strings against a single input
#'
#' Given a single `input` string and multiple `candidates`, compute scores for each candidate.
#'
#' @param input A single string
#' @param candidates One or more candidate strings to score
#' @param .fn The scoring function to use, as a string or function object. Defaults to
#'   [lev_ratio()].
#' @param ... Additional arguments to pass to `.fn`.
#' @param decreasing If `TRUE` (the default), the candidate with the highest score is ranked first.
#'   If using a comparison `.fn` that computes _distance_ rather than similarity, or if you want the
#'   worst match to be returned first, set this to `FALSE`.
#'
#' @return A list where the keys are `candidates` and the values are the scores. The list is sorted
#'   according to the `decreasing` parameter, so by default higher scores are first.
#'
#' @examples
#' lev_score_multiple("bilbo", c("frodo", "gandalf", "legolas"))
#' @export
#' @seealso [lev_best_match()]
lev_score_multiple <- function(input, candidates, .fn = lev_ratio, ..., decreasing = TRUE) {
  if (length(input) > 1) rlang::abort(glue::glue("`input` must be length 1, not {length(input)}"))
  .fn <- match.fun(.fn)
  scores <- sort(sapply(candidates, .fn, input, ...), decreasing = decreasing)
  as.list(scores)
}

#' Get the best matched string from a list of candidates
#'
#' Given an `input` string and multiple `candidates`, return the candidate with the best score as
#' calculated by `.fn`.
#'
#' @inheritParams lev_score_multiple
#' @return A string
#' @seealso [lev_score_multiple()]
#' @examples
#' lev_best_match("bilbo", c("frodo", "gandalf", "legolas"))
#' @export
lev_best_match <- function(input, candidates, .fn = lev_ratio, ..., decreasing = TRUE) {
  scores <- lev_score_multiple(input = input, candidates = candidates, .fn = .fn, ..., decreasing = decreasing)
  names(scores)[[1]]
}
