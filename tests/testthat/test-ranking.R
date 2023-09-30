test_that("`lev_score_multiple()` works", {
  res <- lev_score_multiple("bilbo", c("frodo", "gandalf", "legolas"))
  expect_true(is.list(res))
  expect_length(res, 3)
})

test_that("`lev_best_match()` works", {
  res <- lev_best_match("bilbo", c("frodo", "gandalf", "legolas"))
  expect_length(res, 1)
  expect_equal(res, "frodo")

  res_2 <- lev_best_match("bilbo", c("frodo", "gandalf", "legolas"), decreasing = FALSE)
  expect_equal(res_2, "gandalf")
})
