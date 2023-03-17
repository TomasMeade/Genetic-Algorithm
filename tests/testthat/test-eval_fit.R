test_that("correctly returns (lm) AIC (including for individual with all 0s)", {
  # create toy dataset
  n <- 100
  x_rand <- runif(n)
  x_pred <- runif(n)

  # y is generated according to x_pred
  y <- rnorm(n, 2 * x_pred)
  data <- data.frame(x_rand, x_pred, y)

  # calculate AICs
  aic_no_pred <- AIC(lm(y ~ 1))
  aic_rand <- AIC(lm(y ~ x_rand))
  aic_pred <- AIC(lm(y ~ x_pred))

  expect_equal(c(aic_no_pred, aic_rand, aic_pred),
               eval_fit(c('00', '10', '01'), data, y_ind = 3))
})

test_that("correctly returns (glm) AIC (including for individual with all 0s)", {
  # create toy dataset
  n <- 100
  x_rand <- runif(n)
  x_pred <- runif(n)

  # y is generated according to x_pred
  y <- as.integer((runif(n, min = 0, max = 0.5) + x_pred) > 1)
  data <- data.frame(x_rand, x_pred, y)

  # calculate AICs
  aic_no_pred <- AIC(glm(y ~ 1, family = "binomial"))
  aic_rand <- AIC(glm(y ~ x_rand, family = "binomial"))
  aic_pred <- AIC(glm(y ~ x_pred, family = "binomial"))

  expect_equal(c(aic_no_pred, aic_rand, aic_pred),
               eval_fit(c('00', '10', '01'), data, y_ind = 3, glm_family = binomial()))
})


test_that("works with simple user-provided fitness function", {
  # define alternate fitness function
  fit <- function(ind) {
    # fitness is sum of digits
    sum(as.integer(strsplit(ind, split = '')[[1]]))
  }

  expect_equal(c(1, 1, 2, 3),
               eval_fit(c('100', '010', '011', '111'), objective_fun = fit))
})

