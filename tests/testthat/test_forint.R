context("Format numeric numbers as Hungarian forints") #no longer recommend. Messages will use the name of the file instead.

# load package where we have the function to be tested
library(mr)

test_that("numeric values accepted by forint() function", {
  expect_equal(forint(42), "42 Ft")
  expect_equal(forint(42.5), "42.50 Ft")
})
