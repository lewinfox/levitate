# Scalars should be produced by the distance functions when they receive two length 1 inputs.
expect_scalar <- function(object) {
  act <- quasi_label(rlang::enquo(object), arg = "object")
  act$is_vector <- is.vector(act$val)
  act$len <- length(act$val)
  expect(
    act$is_vector && act$len == 1,
    glue::glue("{act$lab} is not atomic")
  )
}

# If both inputs are longer than 1 we expect matrix output
expect_matrix <- function(object) {
  expect_true(is.matrix(object))
}

# The shape of the output matrix should match the lengths of the inputs
expect_shape <- function(object, nrow, ncol) {
  act <- quasi_label(rlang::enquo(object), arg = "object")
  if (!(is.matrix(act$val) || is.data.frame(act$val))) {
    msg <- glue::glue("{act$lab} is not a matrix")
    fail(msg)
  }
  act$nrow <- nrow(act$val)
  act$ncol <- ncol(act$val)
  expect(
    act$nrow == nrow && act$ncol == ncol,
    glue::glue("{act$lab} has {act$nrow} rows (expected {nrow}) and {act$ncol} columns (expected {ncol})")
  )
}

# The dimnames should match the inputs
expect_dimnames <- function(object, row_names = NULL, col_names = NULL) {
  act <- quasi_label(rlang::enquo(object), arg = "object")
  if (!is.matrix(act$val)) {
    msg <- glue::glue("{act$lab} is not a matrix")
    fail(msg)
  }
  if (is.null(dimnames(act$val))) {
    if (!is.null(row_names) || !is.null(col_names)) {
      msg <- glue::glue("{act$lab} is missing dimnames")
      fail(msg)
    }
    succeed()
    return(invisible(act$val))
  }
  act$row_names <- dimnames(act$val)[[1]]
  act$col_names <- dimnames(act$val)[[2]]
  expect(
    all(act$row_names == row_names) && all(act$col_names == col_names),
    glue::glue("Row or column names of {act$lab} output do not match the inputs.")
  )
}

test_that("`lev_distance()` returns the correct shape output", {
  # Test with inputs of different lengths
  l1 <- "cat"
  l2 <- c("cat", "fish")
  l3 <- c("cat", "fish", "rat")
  l4 <- c("cat", "fish", "rat", "dish")


  # Two scalar inputs give scalar output
  expect_scalar(lev_distance(l1, l1))

  # One scalar and one vector give a vector independent of order
  expect_length(lev_distance(l1, l2), 2)
  expect_length(lev_distance(l2, l1), 2)

  # Ditto for longer inputs
  expect_length(lev_distance(l1, l3), 3)
  expect_length(lev_distance(l4, l1), 4)

  # If pairwise = TRUE then mismatched input lengths should generate an error
  expect_error(lev_distance(l2, l2), NA)
  expect_error(lev_distance(l2, l3), class = "levitate_length_mismatch")
  expect_error(lev_distance(l2, l4), class = "levitate_length_mismatch")

  # If both inputs are longer than 1 then we expect a matrix with length(a) rows and length(b)
  # columns
  expect_matrix(lev_distance(l2, l2, pairwise = FALSE))
  expect_shape(lev_distance(l2, l2, pairwise = FALSE), 2, 2)
  expect_shape(lev_distance(l3, l2, pairwise = FALSE), 3, 2)
  expect_shape(lev_distance(l3, l4, pairwise = FALSE), 3, 4)

  # Dimnames should be present and should match inputs
  expect_dimnames(lev_distance(l3, l4, pairwise = FALSE), l3, l4)
  expect_dimnames(lev_distance(l4, l2, pairwise = FALSE), l4, l2)
})

test_that("`lev_ratio()` returns the correct shape output", {
  # Test with inputs of different lengths
  l1 <- "cat"
  l2 <- c("cat", "fish")
  l3 <- c("cat", "fish", "rat")
  l4 <- c("cat", "fish", "rat", "dish")


  # Two scalar inputs give scalar output
  expect_scalar(lev_ratio(l1, l1))

  # One scalar and one vector give a vector independent of order
  expect_length(lev_ratio(l1, l2), 2)
  expect_length(lev_ratio(l2, l1), 2)

  # Ditto for longer inputs
  expect_length(lev_ratio(l1, l3), 3)
  expect_length(lev_ratio(l4, l1), 4)

  # If pairwise = TRUE then mismatched input lengths should generate an error
  expect_error(lev_ratio(l2, l2), NA)
  expect_error(lev_ratio(l2, l3), class = "levitate_length_mismatch")
  expect_error(lev_ratio(l2, l4), class = "levitate_length_mismatch")

  # If both inputs are longer than 1 then we expect a matrix with length(a) rows and length(b)
  # columns
  expect_matrix(lev_ratio(l2, l2, pairwise = FALSE))
  expect_shape(lev_ratio(l2, l2, pairwise = FALSE), 2, 2)
  expect_shape(lev_ratio(l3, l2, pairwise = FALSE), 3, 2)
  expect_shape(lev_ratio(l3, l4, pairwise = FALSE), 3, 4)

  # Dimnames should be present and should match inputs
  expect_dimnames(lev_ratio(l3, l4, pairwise = FALSE), l3, l4)
  expect_dimnames(lev_ratio(l4, l2, pairwise = FALSE), l4, l2)
})

test_that("`lev_partial_ratio()` returns the correct shape output", {
  # Test with inputs of different lengths
  l1 <- "cat"
  l2 <- c("cat", "fish")
  l3 <- c("cat", "fish", "rat")
  l4 <- c("cat", "fish", "rat", "dish")


  # Two scalar inputs give scalar output
  expect_scalar(lev_partial_ratio(l1, l1))

  # One scalar and one vector give a vector independent of order
  expect_length(lev_partial_ratio(l1, l2), 2)
  expect_length(lev_partial_ratio(l2, l1), 2)

  # Ditto for longer inputs
  expect_length(lev_partial_ratio(l1, l3), 3)
  expect_length(lev_partial_ratio(l4, l1), 4)

  # If pairwise = TRUE then mismatched input lengths should generate an error
  expect_error(lev_partial_ratio(l2, l2), NA)
  expect_error(lev_partial_ratio(l2, l3), class = "levitate_length_mismatch")
  expect_error(lev_partial_ratio(l2, l4), class = "levitate_length_mismatch")

  # If both inputs are longer than 1 then we expect a matrix with length(a) rows and length(b)
  # columns
  expect_matrix(lev_partial_ratio(l2, l2, pairwise = FALSE))
  expect_shape(lev_partial_ratio(l2, l2, pairwise = FALSE), 2, 2)
  expect_shape(lev_partial_ratio(l3, l2, pairwise = FALSE), 3, 2)
  expect_shape(lev_partial_ratio(l3, l4, pairwise = FALSE), 3, 4)

  # Dimnames should be present and should match inputs
  expect_dimnames(lev_partial_ratio(l3, l4, pairwise = FALSE), l3, l4)
  expect_dimnames(lev_partial_ratio(l4, l2, pairwise = FALSE), l4, l2)
})

test_that("`lev_token_sort_ratio()` returns the correct shape output", {
  # Test with inputs of different lengths
  l1 <- "cat"
  l2 <- c("cat", "fish")
  l3 <- c("cat", "fish", "rat")
  l4 <- c("cat", "fish", "rat", "dish")


  # Two scalar inputs give scalar output
  expect_scalar(lev_token_sort_ratio(l1, l1))

  # One scalar and one vector give a vector independent of order
  expect_length(lev_token_sort_ratio(l1, l2), 2)
  expect_length(lev_token_sort_ratio(l2, l1), 2)

  # Ditto for longer inputs
  expect_length(lev_token_sort_ratio(l1, l3), 3)
  expect_length(lev_token_sort_ratio(l4, l1), 4)

  # If pairwise = TRUE then mismatched input lengths should generate an error
  expect_error(lev_token_sort_ratio(l2, l2), NA)
  expect_error(lev_token_sort_ratio(l2, l3), class = "levitate_length_mismatch")
  expect_error(lev_token_sort_ratio(l2, l4), class = "levitate_length_mismatch")

  # If both inputs are longer than 1 then we expect a matrix with length(a) rows and length(b)
  # columns
  expect_matrix(lev_token_sort_ratio(l2, l2, pairwise = FALSE))
  expect_shape(lev_token_sort_ratio(l2, l2, pairwise = FALSE), 2, 2)
  expect_shape(lev_token_sort_ratio(l3, l2, pairwise = FALSE), 3, 2)
  expect_shape(lev_token_sort_ratio(l3, l4, pairwise = FALSE), 3, 4)

  # Dimnames should be present and should match inputs
  expect_dimnames(lev_token_sort_ratio(l3, l4, pairwise = FALSE), l3, l4)
  expect_dimnames(lev_token_sort_ratio(l4, l2, pairwise = FALSE), l4, l2)
})

test_that("`lev_token_set_ratio()` returns the correct shape output", {
  # Test with inputs of different lengths
  l1 <- "cat"
  l2 <- c("cat", "fish")
  l3 <- c("cat", "fish", "rat")
  l4 <- c("cat", "fish", "rat", "dish")


  # Two scalar inputs give scalar output
  expect_scalar(lev_token_set_ratio(l1, l1))

  # One scalar and one vector give a vector independent of order
  expect_length(lev_token_set_ratio(l1, l2), 2)
  expect_length(lev_token_set_ratio(l2, l1), 2)

  # Ditto for longer inputs
  expect_length(lev_token_set_ratio(l1, l3), 3)
  expect_length(lev_token_set_ratio(l4, l1), 4)

  # If pairwise = TRUE then mismatched input lengths should generate an error
  expect_error(lev_token_set_ratio(l2, l2), NA)
  expect_error(lev_token_set_ratio(l2, l3), class = "levitate_length_mismatch")
  expect_error(lev_token_set_ratio(l2, l4), class = "levitate_length_mismatch")

  # If both inputs are longer than 1 then we expect a matrix with length(a) rows and length(b)
  # columns
  expect_matrix(lev_token_set_ratio(l2, l2, pairwise = FALSE))
  expect_shape(lev_token_set_ratio(l2, l2, pairwise = FALSE), 2, 2)
  expect_shape(lev_token_set_ratio(l3, l2, pairwise = FALSE), 3, 2)
  expect_shape(lev_token_set_ratio(l3, l4, pairwise = FALSE), 3, 4)

  # Dimnames should be present and should match inputs
  expect_dimnames(lev_token_set_ratio(l3, l4, pairwise = FALSE), l3, l4)
  expect_dimnames(lev_token_set_ratio(l4, l2, pairwise = FALSE), l4, l2)
})

# When given inputs of different lengths that are not a multiple of each other lev_dist() triggers
# a warning from `pmax()` about partial recycling. This is annoying and should be fixed if possible.
test_that("`lev_ratio()` does not warn about fractional argument recycling", {
  expect_warning(lev_ratio(c("a", "b"), c("a", "b", "c"), pairwise = FALSE), NA)
})
