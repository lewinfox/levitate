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

test_that("weighted functions score identical strings as 1", {
  expect_equal(lev_weighted_token_ratio("abc def", "abc def"), 1)
  expect_equal(lev_weighted_token_set_ratio("abc def", "abc def"), 1)
  expect_equal(lev_weighted_token_set_ratio("abc def", "abc def"), 1)
})

test_that("weighted functions score entirely dissimilar strings as 0", {
  expect_equal(lev_weighted_token_ratio("abc def", "ghi jkl"), 0)
  expect_equal(lev_weighted_token_set_ratio("abc def", "ghi jkl"), 0)
  expect_equal(lev_weighted_token_set_ratio("abc def", "ghi jkl"), 0)
})

test_that("weighted function outputs are always between 0 and 1", {
  between_0_and_1 <- function(x) x >= 0 && x <= 1
  for (i in seq(1, 50)) {
    random_para_a <- stringi::stri_rand_lipsum(1)
    random_para_b <- stringi::stri_rand_lipsum(1)
    for (fn in c(lev_weighted_token_ratio, lev_weighted_token_sort_ratio, lev_weighted_token_set_ratio)) {
      expect_true(between_0_and_1(fn(random_para_a, random_para_b)))
    }
  }
})
