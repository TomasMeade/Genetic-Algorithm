ind_vec <- as.character(rbinom(100000, 1, 0.5))


test_that("Does the Mutation Represent The Mutation Probability 0.01?", {
  prob <- 0.01
  ind_vec_mut <- mutate_all(ind_vec, prob)

  # compute observed rate of mutation
  obs_prob <- mean(ind_vec_mut != ind_vec)

  expect_equal(obs_prob, prob, tolerance = 0.1)
})

test_that("Does the Mutation Represent The Mutation Probability: Prob = 0.001?", {
  prob <- 0.001
  ind_vec_mut <- mutate_all(ind_vec, prob)

  # compute observed rate of mutation
  obs_prob <- mean(ind_vec_mut != ind_vec)

  expect_equal(obs_prob, prob, tolerance = 0.1)
})

