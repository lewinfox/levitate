test_that("`score_multiple()` works", {
  res <- score_multiple("bilbo", c("frodo", "gandalf", "legolas"))
  expect_true(is.list(res))
  expect_length(res, 3)
})

test_that("`best_match()` works", {
  res <- best_match("bilbo", c("frodo", "gandalf", "legolas"))
  expect_length(res, 1)
  expect_equal(res, "frodo")

  res_2 <- best_match("bilbo", c("frodo", "gandalf", "legolas"), decreasing = FALSE)
  expect_equal(res_2, "gandalf")
})
