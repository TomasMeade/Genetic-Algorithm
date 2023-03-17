test_that("a string vector of expected length is returned for standard inputs", {
  n_ind <- 40
  n_var <- 20
  first_gen <- init_first_gen(n_ind, n_var)

  expect_type(first_gen, 'character')
  expect_length(first_gen, n_ind)
  expect_equal(nchar(first_gen), rep(n_var, times = n_ind))
})
