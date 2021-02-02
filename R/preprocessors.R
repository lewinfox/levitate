preprocessor_check_string <- function() {
  function(x) {
    name_x <- deparse(substitute(x))
    if (!is.character(x)) {
      msg <- glue::glue("`{name_x}` must be character, not {typeof(x)}")
      stop(msg, call. = FALSE)
    }
  }
}

preprocessor_remove_regex <- function(regex) {
  function(x) stringr::str_remove_all(x, regex)
}

preprocessor_remove_punctuation <- function() {
  preprocessor_remove_regex("[:punct:]")
}

preprocessor_remove_duplicate_whitespace <- function() {
  function(x) stringr::str_replace_all(x, "[^[:graph:]]+", " ")
}

preprocessor_default <- function() {
  function(x) {
    preprocessor_check_string()(x)
    x <- tolower(x)
    x <- preprocessor_remove_punctuation()(x)
    x <- preprocessor_remove_duplicate_whitespace()(x)
    x
  }
}
