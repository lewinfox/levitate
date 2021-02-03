# String processing functions should not affect case
test_that("string processing functions respect case", {
  x <- "The Quick Brown FoX jumPs oVer thE LAZY DoG"
  tokenised_x <- list(c("The", "Quick", "Brown", "FoX", "jumPs", "oVer", "thE", "LAZY", "DoG"))
  expect_equal(str_tokenise(x), tokenised_x)

  expect_equal(str_all_substrings("FloWERs", 3), c("Flo", "loW", "oWE", "WER", "ERs"))

  expect_equal(str_token_sort("FoX Brown DoG"), "Brown DoG FoX")
})
