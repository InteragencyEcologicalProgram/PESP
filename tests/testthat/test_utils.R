
# %!in% -------------------------------------------------------------------

test_that("%!in% works as logical negation of %in%", {
  x <- 1:5
  expect_equal(x[x %!in% c(2,4)], c(1,3,5))
})