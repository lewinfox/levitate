# ---- lev_weighted_token_ratio() ----
test_that("`lev_weighted_token_ratio()` works when comparing equal numbers of tokens", {
  a <- "bill ltd"
  b <- "jill ltd"

  x <- lev_weighted_token_ratio(a, b)
  y <- lev_weighted_token_ratio(a, b, weights = list(ltd = 0.1))

  expect_true(x > y)
})

test_that("de-weighting shared token reduces score in `lev_weighted_token_ratio()`", {
  expect_true(lev_weighted_token_ratio("google", "google, inc") > lev_weighted_token_ratio("google", "google, inc", weights = list(google = 0.1)))
})

test_that("de-weighting non-shared token increases score in `lev_weighted_token_ratio()`", {
  expect_true(lev_weighted_token_ratio("google", "google, inc") < lev_weighted_token_ratio("google", "google, inc", weights = list(inc = 0.1)))
})

test_that("`lev_weighted_token_ratio()` handles different numbers of tokens", {
  a <- "big tech, inc"
  b <- "big tech"
  x <- lev_weighted_token_ratio(a, b)
  y <- lev_weighted_token_ratio(a, b, weights = list(inc = 0.1))
  expect_true(x < y)
})

test_that("`lev_weighted_token_ratio()` handles zero-weighted tokens correctly", {
  expect_equal(
    lev_weighted_token_ratio("bill ltd", "jill ltd", weights = list(ltd = 0)),
    lev_weighted_token_ratio("bill", "jill")
  )
})
