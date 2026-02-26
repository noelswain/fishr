# tests/testthat/helper.R

# Helper function to generate sample fishing data
generate_fishing_data <- function(n = 10) {
  set.seed(67)
  data.frame(
    catch = runif(n, 10, 500),
    effort = runif(n, 1, 20),
    gear_factor = runif(n, 1, 5)
  )
}


expect_equal_numeric <- function(object, expected, ...) {
  expect_equal(as.numeric(object), expected, ...)
}
